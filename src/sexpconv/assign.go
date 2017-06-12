package sexpconv

import (
	"exn"
	"go/ast"
	"go/token"
	"go/types"
	"magic_pkg/emacs/rt"
	"sexp"
	"sys_info/function"
	"xtypes"
)

// Go blank identifier which allows a value to be evaluated, but discarded.
const blankIdent = "_"

func isBlankIdent(node ast.Expr) bool {
	if node, ok := node.(*ast.Ident); ok {
		return node.Name == blankIdent
	}
	return false
}

func (conv *Converter) AssignStmt(node *ast.AssignStmt) sexp.Form {
	switch node.Tok {
	case token.ADD_ASSIGN:
		return conv.addAssign(node.Lhs[0], node.Rhs[0])
	case token.SUB_ASSIGN:
		return conv.subAssign(node.Lhs[0], node.Rhs[0])
	case token.MUL_ASSIGN:
		return conv.mulAssign(node.Lhs[0], node.Rhs[0])
	case token.QUO_ASSIGN:
		return conv.quoAssign(node.Lhs[0], node.Rhs[0])

	default:
		return conv.genAssign(node.Lhs, node.Rhs)
	}
}

func (conv *Converter) genAssign(lhs, rhs []ast.Expr) sexp.Form {
	if len(lhs) == len(rhs) {
		return conv.singleValueAssign(lhs, rhs)
	}
	return conv.multiValueAssign(lhs, rhs[0])
}

func (conv *Converter) addAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	typ := conv.basicTypeOf(rhs)
	x, y := conv.Expr(lhs), conv.Expr(rhs)
	if typ.Kind() == types.String {
		return conv.assign(lhs, sexp.NewConcat(x, y))
	}
	return conv.assign(lhs, sexp.NewAdd(x, y))
}

func (conv *Converter) subAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	x, y := conv.Expr(lhs), conv.Expr(rhs)
	return conv.assign(lhs, sexp.NewSub(x, y))
}

func (conv *Converter) mulAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	x, y := conv.Expr(lhs), conv.Expr(rhs)
	return conv.assign(lhs, sexp.NewMul(x, y))
}

func (conv *Converter) quoAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	x, y := conv.Expr(lhs), conv.Expr(rhs)
	return conv.assign(lhs, sexp.NewQuo(x, y))
}

func (conv *Converter) rhsMultiValues(rhs ast.Expr) []sexp.Form {
	tuple := conv.typeOf(rhs).(*types.Tuple)
	forms := make([]sexp.Form, tuple.Len())

	// First result is evaluated in a normal way.
	forms[0] = conv.Expr(rhs)

	// Other results are assigned to a global variable.
	// Index uniquely identifies variable used for storage.
	for i := 1; i < tuple.Len(); i++ {
		forms[i] = sexp.Var{
			Name: rt.RetVars[i],
			Typ:  tuple.At(i).Type(),
		}
	}

	return forms
}

func (conv *Converter) multiValueAssign(lhs []ast.Expr, rhs ast.Expr) *sexp.FormList {
	forms := make([]sexp.Form, len(lhs))

	for i, rhs := range conv.rhsMultiValues(rhs) {
		forms[i] = conv.assign(lhs[i], rhs)
	}

	return &sexp.FormList{Forms: forms}
}

func (conv *Converter) singleValueAssign(lhs, rhs []ast.Expr) *sexp.FormList {
	forms := make([]sexp.Form, 0, 1)

	for i := range lhs {
		conv.ctxType = conv.typeOf(lhs[i])
		forms = append(forms, conv.assign(lhs[i], conv.Expr(rhs[i])))
	}

	return &sexp.FormList{Forms: forms}
}

func (conv *Converter) assign(lhs ast.Expr, expr sexp.Form) sexp.Form {
	switch lhs := lhs.(type) {
	case *ast.Ident:
		if lhs.Name == blankIdent {
			return conv.ignoredExpr(expr)
		}
		if conv.info.Defs[lhs] == nil {
			if xtypes.IsGlobal(conv.info.Uses[lhs]) {
				return &sexp.VarUpdate{
					Name: conv.env.InternVar(nil, lhs.Name),
					Expr: conv.valueCopy(expr),
				}
			}
			return &sexp.Rebind{Name: lhs.Name, Expr: conv.valueCopy(expr)}
		}
		return &sexp.Bind{Name: lhs.Name, Init: conv.valueCopy(expr)}

	case *ast.IndexExpr:
		switch typ := conv.typeOf(lhs.X).(type) {
		case *types.Map:
			return &sexp.ExprStmt{
				Expr: conv.lispCall(function.MapInsert, lhs.Index, expr, lhs.X),
			}

		case *types.Array:
			return &sexp.ArrayUpdate{
				Array: conv.Expr(lhs.X),
				Index: conv.Expr(lhs.Index),
				Expr:  uintElem(expr, typ.Elem()),
			}

		case *types.Slice:
			return &sexp.ExprStmt{
				Expr: conv.call(rt.FnSliceSet, lhs.X, lhs.Index, expr),
			}

		default:
			panic(exn.Conv(conv.fileSet, "can't assign to", lhs))
		}

	case *ast.SelectorExpr:
		typ := conv.typeOf(lhs.X)
		if typ, ok := typ.Underlying().(*types.Struct); ok {
			return &sexp.StructUpdate{
				Struct: conv.Expr(lhs.X),
				Index:  xtypes.LookupField(lhs.Sel.Name, typ),
				Expr:   expr,
				Typ:    typ,
			}
		}
		panic(exn.Conv(conv.fileSet, "can't assign to", lhs))

	// #TODO: struct assign, indirect assign
	default:
		panic(exn.Conv(conv.fileSet, "can't assign to", lhs))
	}
}

func (conv *Converter) ignoredExpr(expr sexp.Form) sexp.Form {
	switch expr := expr.(type) {
	case *sexp.Call:
		// Function call can not be ignored because
		// it may have side effects.
		return &sexp.ExprStmt{Expr: expr}

	default:
		// Ignored completely.
		return sexp.EmptyStmt
	}
}
