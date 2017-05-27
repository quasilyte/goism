package compiler

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

func errNode(cl *Compiler, node ast.Node, kind string) Error {
	var buf bytes.Buffer
	printer.Fprint(&buf, cl.pkg.FileSet, node)
	nodePos := cl.pkg.FileSet.Position(node.Pos())
	msg := fmt.Sprintf("%s: %s: `%s' (%T)", nodePos, kind, buf.String(), node)
	return Error{Msg: msg}
}

// This error means that compiler saw a node that is supposed
// to be removed from the AST (by AST simplification).
func errCantCompile(cl *Compiler, node ast.Node) Error {
	return errNode(cl, node, "can't compile")
}

func errUnexpectedExpr(cl *Compiler, node ast.Expr) Error {
	return errNode(cl, node, "unexpected expr")
}

func errUnexpectedStmt(cl *Compiler, node ast.Stmt) Error {
	return errNode(cl, node, "unexpected stmt")
}
