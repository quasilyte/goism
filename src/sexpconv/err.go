package sexpconv

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/printer"
)

type Error struct {
	Msg string
}

func (err Error) Error() string {
	return err.Msg
}

func errNode(conv *Converter, node ast.Node, kind string) Error {
	var buf bytes.Buffer
	printer.Fprint(&buf, conv.fileSet, node)
	nodePos := conv.fileSet.Position(node.Pos())
	msg := fmt.Sprintf(
		"%s:%d: %s: `%s' (%T)",
		nodePos.Filename, nodePos.Line, kind, buf.String(), node,
	)
	return Error{Msg: msg}
}

func errUnexpectedExpr(conv *Converter, expr ast.Expr) Error {
	return errNode(conv, expr, "unexpected expr")
}

func errUnexpectedStmt(conv *Converter, stmt ast.Stmt) Error {
	return errNode(conv, stmt, "unexpected stmt")
}
