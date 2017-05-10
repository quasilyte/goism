package sexpconv

import (
	"go/ast"
	"sexp"
)

func (conv *Converter) ForStmt(node *ast.ForStmt) sexp.Form {
	// for <Init>; <Cond>; <Post> { <Body> }
	if node.Init != nil || node.Post != nil {
		panic("unimplemented")
	}

	return &sexp.While{
		Test: conv.Expr(node.Cond),
		Body: conv.stmtList(node.Body.List),
	}
}
