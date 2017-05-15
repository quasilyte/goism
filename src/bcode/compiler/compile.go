package compiler

import (
	"bcode"
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

func compileCallStmt(cl *Compiler, form sexp.CallStmt) {
	compileCall(cl, form.Call)
	// Discard result only for single result function.
	if form.Fn.ResultKind() == function.ResultSingle {
		emit(cl, bcode.Drop(1))
	}
}

func compileBool(cl *Compiler, form sexp.Bool) {
	if form.Val {
		emit(cl, bcode.ConstRef(cl.cvec.InsertSym("t")))
	} else {
		emit(cl, bcode.ConstRef(cl.cvec.InsertSym("nil")))
	}
}

func compileBinOp(cl *Compiler, instr bcode.Instr, forms [2]sexp.Form) {
	compileExpr(cl, forms[1])
	compileExpr(cl, forms[0])
	emit(cl, instr)
}

func compileBlock(cl *Compiler, form *sexp.Block) {
	compileStmtList(cl, form.Forms)
	cl.st.Drop(uint16(form.Scope.Len()))
}

func compileVar(cl *Compiler, form sexp.Var) {
	emit(cl, bcode.StackRef(cl.st.Ref(form.Name)))
	cl.st.Bind(form.Name)
}

func compileBind(cl *Compiler, form *sexp.Bind) {
	compileExpr(cl, form.Init)
	cl.st.Bind(form.Name)
}

func compileRebind(cl *Compiler, form *sexp.Rebind) {
	compileExpr(cl, form.Expr)
	emit(cl, bcode.StackSet(cl.st.Ref(form.Name)))
}

func compileIf(cl *Compiler, form *sexp.If) {
	if form.Else == nil {
		elseLabel := labelCreate(cl)
		compileExpr(cl, form.Cond)
		emitJmpNil(cl, elseLabel)
		compileBlock(cl, form.Then)
		labelBind(cl, elseLabel)
	} else if sexp.IsReturning(form.Then) {
		elseLabel := labelCreate(cl)
		compileExpr(cl, form.Cond)
		emitJmpNil(cl, elseLabel)
		compileBlock(cl, form.Then)
		labelBind(cl, elseLabel)
		compileStmt(cl, form.Else)
	} else {
		elseLabel := labelCreate(cl)
		endifLabel := labelCreate(cl)
		compileExpr(cl, form.Cond)
		emitJmpNil(cl, elseLabel)
		compileBlock(cl, form.Then)
		emitJmp(cl, endifLabel)
		labelBind(cl, elseLabel)
		compileStmt(cl, form.Else)
		labelBind(cl, endifLabel)
	}
}

func compileReturn(cl *Compiler, form *sexp.Return) {
	switch len(form.Results) {
	case 0:
		emit(cl, bcode.Return(0))
	case 1:
		compileExpr(cl, form.Results[0])
		emit(cl, bcode.Return(1))

	default:
		panic("unimplemented") // #REFS: 1.
	}
}

func compilePanic(cl *Compiler, form *sexp.Panic) {
	call(cl, &function.Panic, form.ErrorData)
}

func compileCall(cl *Compiler, form *sexp.Call) {
	emit(cl, bcode.ConstRef(cl.cvec.InsertSym(form.Fn.Name())))
	compileExprList(cl, form.Args)

	if argc := len(form.Args); form.Fn.ExitsScope() {
		emit(cl, bcode.PanicCall(argc))
	} else {
		switch form.Fn.ResultKind() {
		case function.ResultVoid:
			emit(cl, bcode.VoidCall(argc))
		default:
			emit(cl, bcode.Call(argc))
		}
	}
}
