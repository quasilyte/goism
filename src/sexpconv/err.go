package sexpconv

import (
	"exn"
	"go/ast"
)

func errUnexpectedExpr(conv *converter, expr ast.Expr) exn.Error {
	return exn.Conv(conv.fileSet, "unexpected expr", expr)
}

func errUnexpectedStmt(conv *converter, stmt ast.Stmt) exn.Error {
	return exn.Conv(conv.fileSet, "unexpected stmt", stmt)
}
