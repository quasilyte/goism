package sexpconv

import (
	"go/ast"
	"go/token"
	"go/types"
	"lisp/function"
	"sexp"
)

// Go blank identifier which allows a value to be evaluated, but discarded.
const blankIdent = "_"

// This form produces no executable code. Used as NO-OP.
var emptyForm = &sexp.FormList{}

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
		if len(node.Rhs) == len(node.Lhs) {
			return conv.singleValueAssign(node)
		}
		return conv.multiValueAssign(node)
	}
}

func (conv *Converter) addAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	typ := conv.basicTypeOf(rhs)
	args := [2]sexp.Form{conv.Expr(lhs), conv.Expr(rhs)}

	if typ.Info()&types.IsNumeric != 0 {
		return conv.assign(lhs, &sexp.NumAdd{Args: args})
	}
	if typ.Kind() == types.String {
		return conv.assign(lhs, &sexp.Concat{Args: args[:]})
	}

	panic("unimplemented")
}

func (conv *Converter) subAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	return conv.assign(lhs, &sexp.NumSub{
		Args: [2]sexp.Form{conv.Expr(lhs), conv.Expr(rhs)},
	})
}

func (conv *Converter) mulAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	return conv.assign(lhs, &sexp.NumMul{
		Args: [2]sexp.Form{conv.Expr(lhs), conv.Expr(rhs)},
	})
}

func (conv *Converter) quoAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	return conv.assign(lhs, &sexp.NumQuo{
		Args: [2]sexp.Form{conv.Expr(lhs), conv.Expr(rhs)},
	})
}

func (conv *Converter) multiValueAssign(node *ast.AssignStmt) *sexp.FormList {
	forms := make([]sexp.Form, len(node.Lhs))

	tuple := conv.typeOf(node.Rhs[0]).(*types.Tuple)

	// First result is returned in a normal way.
	if isBlankIdent(node.Lhs[0]) {
		// Do a call, but discard the return value.
		forms[0] = sexp.CallStmt{
			Call: conv.Expr(node.Rhs[0]).(*sexp.Call),
		}
	} else {
		forms[0] = conv.assign(node.Lhs[0], conv.Expr(node.Rhs[0]))
	}

	// Other results are assigned to a global variable.
	// Index uniquely identifies variable used for storage.
	for i := 1; i < len(node.Lhs); i++ {
		forms[i] = conv.assign(node.Lhs[i], &sexp.MultiValueRef{
			Index: i,
			Typ:   tuple.At(i).Type(),
		})
	}

	return &sexp.FormList{Forms: forms}
}

func (conv *Converter) singleValueAssign(node *ast.AssignStmt) *sexp.FormList {
	forms := make([]sexp.Form, 0, 1)

	for i, lhs := range node.Lhs {
		conv.ctxType = conv.typeOf(lhs)
		rhs := conv.Expr(node.Rhs[i])
		forms = append(forms, conv.assign(lhs, rhs))
	}

	return &sexp.FormList{Forms: forms}
}

func (conv *Converter) assign(lhs ast.Expr, expr sexp.Form) sexp.Form {
	switch lhs := lhs.(type) {
	case *ast.Ident:
		if lhs.Name == blankIdent {
			return conv.ignoredExpr(expr)
		}
		if def := conv.info.Defs[lhs]; def == nil {
			return &sexp.Rebind{Name: lhs.Name, Expr: expr}
		}
		return &sexp.Bind{Name: lhs.Name, Init: conv.valueCopy(expr)}

	case *ast.IndexExpr:
		switch typ := conv.typeOf(lhs.X); typ.(type) {
		case *types.Map:
			call := &sexp.Call{
				Fn: function.MapInsert,
				Args: conv.valueCopyList([]sexp.Form{
					conv.Expr(lhs.Index),
					expr,
					conv.Expr(lhs.X),
				}),
			}
			return sexp.CallStmt{Call: call}

		case *types.Array:
			return &sexp.ArrayUpdate{
				Array: conv.Expr(lhs.X),
				Index: conv.Expr(lhs.Index),
				Expr:  expr,
			}

		case *types.Slice:
			slice := conv.Expr(lhs.X)
			index := conv.Expr(lhs.Index)
			if sexp.Cost(slice) > 4 {
				return _let(slice, &sexp.SliceUpdate{
					Slice: _it(slice.Type()),
					Index: index,
					Expr:  expr,
				})
			}
			return &sexp.SliceUpdate{Slice: slice, Index: index, Expr: expr}

		default:
			panic("unimplemented")
		}

	// #TODO: struct assign, indirect assign
	default:
		panic("unimplemented")
	}
}

func (conv *Converter) ignoredExpr(expr sexp.Form) sexp.Form {
	switch expr := expr.(type) {
	case *sexp.Call:
		// Function call can not be ignored because
		// it may have side effects.
		return sexp.CallStmt{Call: expr}

	default:
		// Ignored completely.
		return emptyForm
	}
}
