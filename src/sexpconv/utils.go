package sexpconv

import (
	"go/ast"
	"sexp"
)

func (conv *converter) exprList(nodes []ast.Expr) []sexp.Form {
	forms := make([]sexp.Form, len(nodes))
	for i, node := range nodes {
		forms[i] = conv.Expr(node)
	}
	return forms
}

func (conv *converter) stmtList(nodes []ast.Stmt) []sexp.Form {
	forms := make([]sexp.Form, len(nodes))
	for i, node := range nodes {
		forms[i] = conv.Stmt(node)
	}
	return forms
}

func isStructLit(form sexp.Form) bool {
	_, ok := form.(*sexp.StructLit)
	return ok
}

func isArrayLit(form sexp.Form) bool {
	_, ok := form.(*sexp.ArrayLit)
	if ok {
		return true
	}
	_, ok = form.(*sexp.SparseArrayLit)
	return ok
}
