package sexpconv

import (
	"fmt"
	"go/ast"
	"go/types"
	"sexp"
)

func Stmt(info *types.Info, node ast.Stmt) sexp.Form {
	switch node := node.(type) {
	case *ast.IfStmt:
		return IfStmt(info, node)
	case *ast.ReturnStmt:
		return ReturnStmt(info, node)
	case *ast.BlockStmt:
		return BlockStmt(info, node)

	default:
		panic(fmt.Sprintf("unexpected stmt: %#v\n", node))
	}
}

func IfStmt(info *types.Info, node *ast.IfStmt) *sexp.If {
	if node.Init != nil {
		panic("unimplemented")
	}

	test := Expr(info, node.Cond)
	then := Stmt(info, node.Body)
	form := &sexp.If{Test: test, Then: then}
	if node.Else != nil {
		form.Else = Stmt(info, node.Else)
	}

	return form
}

func ReturnStmt(info *types.Info, node *ast.ReturnStmt) *sexp.Return {
	return &sexp.Return{Results: exprList(info, node.Results)}
}

func BlockStmt(info *types.Info, node *ast.BlockStmt) *sexp.Block {
	return &sexp.Block{Forms: stmtList(info, node.List)}
}
