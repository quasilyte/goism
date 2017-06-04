package compiler

import (
	"assert"
	"ir/instr"
	"lisp/rt"
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

func compileRepeat(cl *Compiler, form *sexp.Repeat) {
	assert.True(form.N <= 256)
	for i := int64(0); i < form.N; i++ {
		compileBlock(cl, form.Body)
	}
}

func compileWhile(cl *Compiler, form *sexp.While) {
	condLabel := labelCreate(cl, "loop-cond")
	bodyLabel := labelCreate(cl, "loop-body")
	emitJmp(cl, condLabel)
	labelBind(cl, bodyLabel)
	compileBlock(cl, form.Body)
	labelBind(cl, condLabel)
	compileExpr(cl, form.Cond)
	emitJmpNotNil(cl, bodyLabel)
}

func compileBind(cl *Compiler, form *sexp.Bind) {
	compileExpr(cl, form.Init)
	cl.st.Bind(form.Name)
}

func compileRebind(cl *Compiler, name string, expr sexp.Form) {
	compileExpr(cl, expr)
	stIndex := cl.st.Find(name)
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

func compileSliceUpdate(cl *Compiler, form *sexp.SliceUpdate) {
	compileExpr(cl, form.Slice) // <slice>
	emit(cl, instr.Car)         // <data>
	compileExpr(cl, form.Index) // <data index>
	compileExpr(cl, form.Slice) // <data index slice>
	emit(cl, instr.Cdr)         // <data index cdr(slice)>
	emit(cl, instr.Car)         // <data index offset>
	emit(cl, instr.NumAdd)      // <data real-index>
	compileExpr(cl, form.Expr)  // <data real-index val>
	emit(cl, instr.ArraySet)    // <>
}

func compileStructUpdate(cl *Compiler, form *sexp.StructUpdate) {
	switch rt.StructReprOf(form.Typ) {
	case rt.StructAtom:
		s := form.Struct.(sexp.Var)
		if cl.st.Find(s.Name) != -1 {
			compileRebind(cl, s.Name, form.Expr)
		} else {
			compileVarUpdate(cl, s.Name, form.Expr)
		}

	case rt.StructCons:
		compileExpr(cl, form.Struct)
		emitN(cl, instr.Cdr, form.Index-1)
		compileExpr(cl, form.Expr)
		if form.Typ.NumFields() == form.Index+1 { // Last index.
			emit(cl, instr.SetCdr)
		} else {
			emit(cl, instr.SetCar)
		}

	case rt.StructVec:
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
