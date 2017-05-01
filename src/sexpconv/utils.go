package sexpconv

import (
	"go/ast"
	"go/types"
	"sexp"
)

func exprList(info *types.Info, nodes []ast.Expr) []sexp.Node {
	forms := make([]sexp.Node, len(nodes))
	for i, node := range nodes {
		forms[i] = Expr(info, node)
	}
	return forms
}

func stmtList(info *types.Info, nodes []ast.Stmt) []sexp.Node {
	forms := make([]sexp.Node, len(nodes))
	for i, node := range nodes {
		forms[i] = Stmt(info, node)
	}
	return forms
}
