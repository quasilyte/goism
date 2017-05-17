package compiler

import (
	"ir"
	"lisp/function"
	"sexp"
)

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

func compileBlock(cl *Compiler, form *sexp.Block) {
	compileStmtList(cl, form.Forms)
	cl.st.Discard(uint16(form.Scope.Len()))
}

func compileReturn(cl *Compiler, form *sexp.Return) {
	switch len(form.Results) {
	case 0:
		emit(cl, ir.Return(0))
	case 1:
		compileExpr(cl, form.Results[0])
		emit(cl, ir.Return(1))

	default:
		panic("unimplemented") // #REFS: 1.
	}
}

func compileIf(cl *Compiler, form *sexp.If) {
	if form.Else == nil {
		elseLabel := labelCreate(cl, "else")
		compileExpr(cl, form.Cond)
		emitJmpNil(cl, elseLabel)
		compileBlock(cl, form.Then)
		labelBind(cl, elseLabel)
	} else {
		elseLabel := labelCreate(cl, "else")
		endifLabel := labelCreate(cl, "endif")
		compileExpr(cl, form.Cond)
		emitJmpNil(cl, elseLabel)
		compileBlock(cl, form.Then)
		emitJmp(cl, endifLabel)
		labelBind(cl, elseLabel)
		compileStmt(cl, form.Else)
		labelBind(cl, endifLabel)
	}
}

func compileBind(cl *Compiler, form *sexp.Bind) {
	compileExpr(cl, form.Init)
	cl.st.Bind(form.Name)
}

func compileRebind(cl *Compiler, form *sexp.Rebind) {
	compileExpr(cl, form.Expr)
	emit(cl, ir.StackSet(cl.st.Find(form.Name)))
}

func compileCallStmt(cl *Compiler, form sexp.CallStmt) {
	compileCall(cl, form.Call)

	if form.Fn.IsPanic() {
		return
	}

	switch form.Fn.ResultKind() {
	case function.ResultMulti, function.ResultVoid:
		/* Result already discarded by compileCall */

	default:
		emit(cl, ir.Discard(1))
	}
}

func compileBinOp(cl *Compiler, instr ir.Instr, forms [2]sexp.Form) {
	compileExpr(cl, forms[1])
	compileExpr(cl, forms[0])
	emit(cl, instr)
}

func compileBool(cl *Compiler, form sexp.Bool) {
	if form.Val {
		emit(cl, ir.ConstRef(cl.cvec.InsertSym("t")))
	} else {
		emit(cl, ir.ConstRef(cl.cvec.InsertSym("nil")))
	}
}

func compileVar(cl *Compiler, form sexp.Var) {
	emit(cl, ir.StackRef(cl.st.Find(form.Name)))
	cl.st.Bind(form.Name)
}

func compilePanic(cl *Compiler, form *sexp.Panic) {
	call(cl, &function.Panic, form.ErrorData)
}

func compileCall(cl *Compiler, form *sexp.Call) {
	emit(cl, ir.ConstRef(cl.cvec.InsertSym(form.Fn.Name())))
	compileExprList(cl, form.Args)

	if argc := len(form.Args); form.Fn.IsPanic() {
		emit(cl, ir.PanicCall(argc))
	} else {
		switch form.Fn.ResultKind() {
		case function.ResultMulti, function.ResultVoid:
			emit(cl, ir.VoidCall(argc))

		default:
			emit(cl, ir.Call(argc))
		}
	}
}
