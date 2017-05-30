package sexpconv

import (
	"exn"
	"go/ast"
)

func errUnexpectedExpr(conv *Converter, expr ast.Expr) exn.Error {
	return exn.Conv(conv.fileSet, "unexpected expr", expr)
}

func errUnexpectedStmt(conv *Converter, stmt ast.Stmt) exn.Error {
	return exn.Conv(conv.fileSet, "unexpected stmt", stmt)
}
