package compiler

import (
	"sexp"
)

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

func compileInt(cl *Compiler, val int64) {
	cl.push().ConstRef(cl.cvec.InsertInt(val))
}

func compileFloat(cl *Compiler, val float64) {
	cl.push().ConstRef(cl.cvec.InsertFloat(val))
}

func compileString(cl *Compiler, val string) {
	cl.push().ConstRef(cl.cvec.InsertString(val))
}

func compileSym(cl *Compiler, name string) {
	cl.push().ConstRef(cl.cvec.InsertSym(name))
}
