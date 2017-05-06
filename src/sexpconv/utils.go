package sexpconv

import (
	"go/ast"
	"sexp"
)

func (conv *Converter) exprList(nodes []ast.Expr) []sexp.Form {
	forms := make([]sexp.Form, len(nodes))
	for i, node := range nodes {
		forms[i] = conv.Expr(node)
	}
	return forms
}

func (conv *Converter) stmtList(nodes []ast.Stmt) []sexp.Form {
	forms := make([]sexp.Form, len(nodes))
	for i, node := range nodes {
		forms[i] = conv.Stmt(node)
	}
	return forms
}

func (conv *Converter) call(fn string, args ...ast.Expr) *sexp.Call {
	sexpArgs := make([]sexp.Form, len(args))
	for i, arg := range args {
		sexpArgs[i] = conv.Expr(arg)
	}
	return &sexp.Call{
		Fn:   fn,
		Args: sexpArgs,
	}
}
