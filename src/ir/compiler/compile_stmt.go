package compiler

import (
	"assert"
	"ir/instr"
	"lang"
	"magic_pkg/emacs/rt"
	"sexp"
)

func compileBlock(cl *Compiler, form *sexp.Block) {
	depth := cl.st.Len()
	compileStmtList(cl, form.Forms)
	if scopeSize := cl.st.Len() - depth; scopeSize != 0 {
		emit(cl, instr.Discard(scopeSize))
	}
}

func compileReturn(cl *Compiler, form *sexp.Return) {
	if len(form.Results) == 0 {
		// Any function in Emacs Lisp must return a value.
		// To avoid Emacs crash, we always return "nil" for void functions.
		emit(cl, instr.ConstRef(cl.cvec.InsertSym("nil")))
		emit(cl, instr.Return)
	} else {
		compileExpr(cl, form.Results[0])
		for i := 1; i < len(form.Results); i++ {
			compileExpr(cl, form.Results[i])
			sym := rt.RetVars[i]
			emit(cl, instr.VarSet(cl.cvec.InsertSym(sym)))
		}
		emit(cl, instr.Return)
	}
}

func compileIf(cl *Compiler, form *sexp.If) {
	if form.Else == nil {
		endifLabel := labelCreate(cl, "endif")
		compileExpr(cl, form.Cond)
		emitJmpNil(cl, endifLabel)
		compileBlock(cl, form.Then)
		labelBind(cl, endifLabel)
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

func compileRepeat(cl *Compiler, form *sexp.Repeat) {
	assert.True(form.N <= 256)
	for i := int64(0); i < form.N; i++ {
		compileBlock(cl, form.Body)
	}
}

func compileWhile(cl *Compiler, form *sexp.While) {
	condLabel := labelCreate(cl, "while-cond")
	bodyLabel := labelCreate(cl, "while-body")
	breakLabel := labelCreate(cl, "while-break")
	continueLabel := labelCreate(cl, "while-continue")

	prevBreak := cl.innerBreak
	prevContinue := cl.innerContinue
	cl.innerBreak = breakLabel
	cl.innerContinue = continueLabel

	emitJmp(cl, condLabel)
	labelBind(cl, bodyLabel)
	compileBlock(cl, form.Body)
	labelBind(cl, continueLabel)
	compileStmt(cl, form.Post)
	labelBind(cl, condLabel)
	if form.Cond == nil {
		emitJmp(cl, bodyLabel)
	} else {
		compileExpr(cl, form.Cond)
		emitJmpNotNil(cl, bodyLabel)
	}
	labelBind(cl, breakLabel)

	cl.innerBreak = prevBreak
	cl.innerContinue = prevContinue
}

func compileBind(cl *Compiler, form *sexp.Bind) {
	compileExpr(cl, form.Init)
	cl.st.Bind(form.Name)
}

func compileRebind(cl *Compiler, name string, expr sexp.Form) {
	compileExpr(cl, expr)
	stIndex := cl.st.Lookup(name)
	emit(cl, instr.StackSet(stIndex))
	// "-1" because we have just popped stask element.
	cl.st.Rebind(stIndex-1, name)
}

func compileVarUpdate(cl *Compiler, name string, expr sexp.Form) {
	compileExpr(cl, expr)
	emit(cl, instr.VarSet(cl.cvec.InsertSym(name)))
}

func compileExprStmt(cl *Compiler, form *sexp.ExprStmt) {
	compileExpr(cl, form.Expr)

	if form, ok := form.Expr.(*sexp.InstrCall); ok {
		if form.Instr.Output != instr.AttrPushTmp {
			return // No cleanup is needed
		}
	}

	if sexp.IsThrow(form.Expr) {
		cl.st.Discard(1)
	} else {
		emit(cl, instr.Discard(1))
	}
}

func compileArrayUpdate(cl *Compiler, form *sexp.ArrayUpdate) {
	compileExpr(cl, form.Array)
	compileExpr(cl, form.Index)
	compileExpr(cl, form.Expr)
	emit(cl, instr.ArraySet)
}

func compileStructUpdate(cl *Compiler, form *sexp.StructUpdate) {
	switch lang.StructReprOf(form.Typ) {
	case lang.StructUnit:
		compileExpr(cl, form.Struct)
		compileExpr(cl, form.Expr)
		emit(cl, instr.SetCar)

	case lang.StructCons:
		compileExpr(cl, form.Struct)
		if form.Index == 0 {
			// First member.
			compileExpr(cl, form.Expr)
			emit(cl, instr.SetCar)
		} else if form.Typ.NumFields() == form.Index+1 {
			// Last member.
			emitN(cl, instr.Cdr, form.Index-1)
			compileExpr(cl, form.Expr)
			emit(cl, instr.SetCdr)
		} else {
			emitN(cl, instr.Cdr, form.Index)
			compileExpr(cl, form.Expr)
			emit(cl, instr.SetCar)
		}

	case lang.StructVec:
		compileArrayUpdate(cl, &sexp.ArrayUpdate{
			Array: form.Struct,
			Index: sexp.Int(form.Index),
			Expr:  form.Expr,
		})
	}
}

func compileLetStmt(cl *Compiler, form *sexp.Let) {
	for _, bind := range form.Bindings {
		compileBind(cl, bind)
	}
	compileStmt(cl, form.Stmt)
	emit(cl, instr.Discard(len(form.Bindings)))
}

func compileGoto(cl *Compiler, form *sexp.Goto) {
	switch name := form.LabelName; name {
	case "continue":
		emitJmp(cl, cl.innerContinue)
	case "break":
		emitJmp(cl, cl.innerBreak)
	default:
		emitJmp(cl, label("&"+form.LabelName))
	}
}

func compileLabel(cl *Compiler, form *sexp.Label) {
	labelBind(cl, label("&"+form.Name))
}
