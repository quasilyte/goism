package sexpconv

import (
	"go/ast"
	"go/token"
	"go/types"
	"sexp"
)

func AssignStmt(info *types.Info, node *ast.AssignStmt) sexp.Form {
	switch node.Tok {
	case token.ADD_ASSIGN:
		return addAssign(info, node.Lhs[0], node.Rhs[0])
	case token.SUB_ASSIGN:
		return subAssign(info, node.Lhs[0], node.Rhs[0])
	case token.MUL_ASSIGN:
		return mulAssign(info, node.Lhs[0], node.Rhs[0])
	case token.QUO_ASSIGN:
		return quoAssign(info, node.Lhs[0], node.Rhs[0])

	default:
		if len(node.Rhs) != len(node.Lhs) {
			return multiValueAssign(info, node)
		}
		return singleValueAssign(info, node)
	}
}

func addAssign(info *types.Info, lhs ast.Expr, rhs ast.Expr) sexp.Form {
	typ := info.TypeOf(rhs).(*types.Basic)
	args := []sexp.Form{Expr(info, lhs), Expr(info, rhs)}

	if typ.Info()&types.IsNumeric != 0 {
		return assign(info, lhs, &sexp.NumAdd{Type: typ, Args: args})
	}
	if typ.Kind() == types.String {
		return assign(info, lhs, &sexp.Concat{Args: args})
	}

	panic("unimplemented")
}

func subAssign(info *types.Info, lhs ast.Expr, rhs ast.Expr) sexp.Form {
	return assign(info, lhs, &sexp.NumSub{
		Type: info.TypeOf(rhs).(*types.Basic),
		Args: []sexp.Form{Expr(info, lhs), Expr(info, rhs)},
	})
}

func mulAssign(info *types.Info, lhs ast.Expr, rhs ast.Expr) sexp.Form {
	return assign(info, lhs, &sexp.NumMul{
		Type: info.TypeOf(rhs).(*types.Basic),
		Args: []sexp.Form{Expr(info, lhs), Expr(info, rhs)},
	})
}

func quoAssign(info *types.Info, lhs ast.Expr, rhs ast.Expr) sexp.Form {
	return assign(info, lhs, &sexp.NumQuo{
		Type: info.TypeOf(rhs).(*types.Basic),
		Args: []sexp.Form{Expr(info, lhs), Expr(info, rhs)},
	})
}

func multiValueAssign(info *types.Info, node *ast.AssignStmt) *sexp.FormList {
	panic("unimplemented")
}

func singleValueAssign(info *types.Info, node *ast.AssignStmt) *sexp.FormList {
	forms := make([]sexp.Form, 0, 1)

	for i, lhs := range node.Lhs {
		rhs := Expr(info, node.Rhs[i])
		forms = append(forms, assign(info, lhs, rhs))
	}

	return &sexp.FormList{Forms: forms}
}

func assign(info *types.Info, lhs ast.Expr, expr sexp.Form) sexp.Form {
	if lhs, ok := lhs.(*ast.Ident); ok {
		if def := info.Defs[lhs]; def == nil {
			return &sexp.Rebind{Name: lhs.Name, Expr: expr}
		}
		return &sexp.Bind{Name: lhs.Name, Init: expr}
	}

	// #TODO: struct assign, indirect assign, index assign.
	panic("unimplemented")
}
