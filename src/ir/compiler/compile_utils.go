package compiler

import "sexp"

func call(cl *Compiler, name string, args ...sexp.Form) {
	compileCall(cl, name, args)
}

func compileStmtList(cl *Compiler, forms []sexp.Form) {
	for _, form := range forms {
		compileStmt(cl, form)
	}
}

func compileExprList(cl *Compiler, forms []sexp.Form) {
	for _, form := range forms {
		compileExpr(cl, form)
	}
}
