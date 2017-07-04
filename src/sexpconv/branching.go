package sexpconv

import (
	"go/ast"
	"go/token"
	"sexp"
)

func (conv *converter) BranchStmt(node *ast.BranchStmt) sexp.Form {
	switch node.Tok {
	case token.CONTINUE:
		if node.Label != nil {
			panic(errUnexpectedStmt(conv, node)) // #REFS: 66
		}
		return sexp.ContinueGoto

	case token.BREAK:
		if node.Label != nil {
			panic(errUnexpectedStmt(conv, node)) // #REFS: 66
		}
		return sexp.BreakGoto

	case token.GOTO:
		return &sexp.Goto{LabelName: node.Label.Name}

	default:
		panic(errUnexpectedStmt(conv, node))
	}
}

func (conv *converter) LabeledStmt(node *ast.LabeledStmt) sexp.Form {
	return sexp.FormList([]sexp.Form{
		&sexp.Label{Name: node.Label.Name},
		conv.Stmt(node.Stmt),
	})
}
