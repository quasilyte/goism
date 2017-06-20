package sexpconv

import (
	"go/ast"
	"sexp"
)

func (conv *converter) SwitchStmt(node *ast.SwitchStmt) sexp.Form {
	return conv.withInitStmt(node.Init, conv.switchStmt(node))
}

func (conv *converter) switchStmt(node *ast.SwitchStmt) sexp.Form {
	defaultBody := sexp.EmptyBlock
	clauses := make([]sexp.CaseClause, 0, len(node.Body.List))

	for _, cc := range node.Body.List {
		cc := cc.(*ast.CaseClause)
		body := &sexp.Block{Forms: conv.stmtList(cc.Body)}
		if cc.List == nil {
			defaultBody = body
		} else {
			for _, caseExpr := range cc.List {
				clauses = append(clauses, sexp.CaseClause{
					Expr: conv.Expr(caseExpr),
					Body: body,
				})
			}
		}
	}

	body := sexp.SwitchBody{
		Clauses:     clauses,
		DefaultBody: defaultBody,
	}
	if node.Tag == nil {
		return &sexp.SwitchTrue{SwitchBody: body}
	}
	return &sexp.Switch{
		Expr:       conv.Expr(node.Tag),
		SwitchBody: body,
	}
}
