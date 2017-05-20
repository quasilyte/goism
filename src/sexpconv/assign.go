package sexpconv

import (
	"go/ast"
	"go/token"
	"go/types"
	"lisp/function"
	"sexp"
)

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
		return conv.assign(lhs, &sexp.NumAdd{Type: typ, Args: args})
	}
	if typ.Kind() == types.String {
		return conv.assign(lhs, &sexp.Concat{Args: args})
	}

	panic("unimplemented")
}

func (conv *Converter) subAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	return conv.assign(lhs, &sexp.NumSub{
		Type: conv.basicTypeOf(rhs),
		Args: [2]sexp.Form{conv.Expr(lhs), conv.Expr(rhs)},
	})
}

func (conv *Converter) mulAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	return conv.assign(lhs, &sexp.NumMul{
		Type: conv.basicTypeOf(rhs),
		Args: [2]sexp.Form{conv.Expr(lhs), conv.Expr(rhs)},
	})
}

func (conv *Converter) quoAssign(lhs ast.Expr, rhs ast.Expr) sexp.Form {
	return conv.assign(lhs, &sexp.NumQuo{
		Type: conv.basicTypeOf(rhs),
		Args: [2]sexp.Form{conv.Expr(lhs), conv.Expr(rhs)},
	})
}

func (conv *Converter) multiValueAssign(node *ast.AssignStmt) *sexp.FormList {
	forms := make([]sexp.Form, len(node.Lhs))

	// First result returned in a normal way.
	forms[0] = conv.assign(node.Lhs[0], conv.Expr(node.Rhs[0]))

	// Other results assigned to a global variable.
	// Index uniquely identifies variable used for storage.
	for i := 1; i < len(node.Lhs); i++ {
		forms[i] = conv.assign(node.Lhs[i], &sexp.MultiValueRef{Index: i})
	}

	return &sexp.FormList{Forms: forms}
}

func (conv *Converter) singleValueAssign(node *ast.AssignStmt) *sexp.FormList {
	forms := make([]sexp.Form, 0, 1)

	for i, lhs := range node.Lhs {
		rhs := conv.Expr(node.Rhs[i])
		forms = append(forms, conv.assign(lhs, rhs))
	}

	return &sexp.FormList{Forms: forms}
}

func (conv *Converter) assign(lhs ast.Expr, expr sexp.Form) sexp.Form {
	switch lhs := lhs.(type) {
	case *ast.Ident:
		if def := conv.info.Defs[lhs]; def == nil {
			return &sexp.Rebind{Name: lhs.Name, Expr: expr}
		}
		return &sexp.Bind{Name: lhs.Name, Init: expr}

	case *ast.IndexExpr:
		switch typ := conv.typeOf(lhs.X); typ.(type) {
		case *types.Map:
			call := &sexp.Call{
				Fn: &function.MapInsert,
				Args: []sexp.Form{
					conv.Expr(lhs.Index),
					expr,
					conv.Expr(lhs.X),
				},
			}
			return sexp.CallStmt{Call: call}

		case *types.Array:
			return &sexp.ArrayUpdate{
				Array: conv.Expr(lhs.X),
				Index: conv.Expr(lhs.Index),
				Expr:  expr,
			}

		default:
			panic("unimplemented")
		}

	// #TODO: struct assign, indirect assign
	default:
		panic("unimplemented")
	}
}
