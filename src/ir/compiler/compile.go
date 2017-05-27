package compiler

import (
	"ir/instr"
	"lisp"
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
	if form.Scope.Len() != 0 {
		emit(cl, instr.Discard(form.Scope.Len()))
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
			sym := lisp.RetVars[i]
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
	assert(form.N <= 256)
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

func compileRebind(cl *Compiler, form *sexp.Rebind) {
	compileExpr(cl, form.Expr)
	if stIndex := cl.st.Find(form.Name); stIndex != -1 {
		emit(cl, instr.StackSet(stIndex))
		// "-1" because we popped stask element.
		cl.st.Rebind(stIndex-1, form.Name)
	} else {
		emit(cl, instr.VarSet(cl.cvec.InsertSym(form.Name)))
	}
}

func compileCallStmt(cl *Compiler, form sexp.CallStmt) {
	compileCall(cl, form.Fn.Name(), form.Args)

	if form.Fn.IsPanic() {
		cl.st.Discard(1)
	} else {
		emit(cl, instr.Discard(1))
	}
}

func compileBinOp(cl *Compiler, ins instr.Instr, args [2]sexp.Form) {
	compileExpr(cl, args[0])
	compileExpr(cl, args[1])
	emit(cl, ins)
}

func compileUnaryOp(cl *Compiler, ins instr.Instr, arg sexp.Form) {
	compileExpr(cl, arg)
	emit(cl, ins)
}

func compileBool(cl *Compiler, form sexp.Bool) {
	if bool(form) {
		emit(cl, instr.ConstRef(cl.cvec.InsertSym("t")))
	} else {
		emit(cl, instr.ConstRef(cl.cvec.InsertSym("nil")))
	}
}

func compileVar(cl *Compiler, form sexp.Var) {
	if stIndex := cl.st.Find(form.Name); stIndex != -1 {
		emit(cl, instr.StackRef(stIndex))
	} else {
		emit(cl, instr.VarRef(cl.cvec.InsertSym(form.Name)))
	}
	cl.st.Bind(form.Name)
}

func compileSparseArrayLit(cl *Compiler, form *sexp.SparseArrayLit) {
	compileCall(cl, form.Ctor.Fn.Name(), form.Ctor.Args)
	for _, val := range form.Vals {
		emit(cl, instr.StackRef(0)) // Array
		emit(cl, instr.ConstRef(cl.cvec.InsertInt(val.Index)))
		compileExpr(cl, val.Expr)
		emit(cl, instr.ArraySet)
	}
}

func compileCall(cl *Compiler, name string, args []sexp.Form) {
	emit(cl, instr.ConstRef(cl.cvec.InsertSym(name)))
	compileExprList(cl, args)
	emit(cl, instr.Call(len(args)))
}

func compileArrayIndex(cl *Compiler, form *sexp.ArrayIndex) {
	compileExpr(cl, form.Array)
	compileExpr(cl, form.Index)
	emit(cl, instr.ArrayRef)
}

func compileArrayUpdate(cl *Compiler, form *sexp.ArrayUpdate) {
	compileExpr(cl, form.Array)
	compileExpr(cl, form.Index)
	compileExpr(cl, form.Expr)
	emit(cl, instr.ArraySet)
}

func compileLetExpr(cl *Compiler, form *sexp.Let) {
	compileBind(cl, form.Bind)
	compileExpr(cl, form.Expr)
	emit(cl, instr.StackSet(1)) // Replace let binding with expr result
}

func compileLetStmt(cl *Compiler, form *sexp.Let) {
	compileBind(cl, form.Bind)
	compileStmt(cl, form.Stmt)
	emit(cl, instr.Discard(1)) // Remove let binding
}

func compileSliceIndex(cl *Compiler, form *sexp.SliceIndex) {
	compileExpr(cl, form.Slice) // <slice>
	emit(cl, instr.Car)         // <data>
	compileExpr(cl, form.Slice) // <data slice>
	emit(cl, instr.Cdr)         // <data cdr(slice)>
	emit(cl, instr.Car)         // <data offset>
	compileExpr(cl, form.Index) // <data offset index>
	emit(cl, instr.NumAdd)      // <data real-index>
	emit(cl, instr.ArrayRef)    // <elem>
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

func compileSliceLen(cl *Compiler, form *sexp.UnaryOp) {
	compileExpr(cl, form.X)
	emit(cl, instr.Cdr)
	emit(cl, instr.Cdr)
	emit(cl, instr.Car)
}

func compileSliceCap(cl *Compiler, form *sexp.UnaryOp) {
	compileExpr(cl, form.X)
	emit(cl, instr.Cdr)
	emit(cl, instr.Cdr)
	emit(cl, instr.Cdr)
}

func compileSubslice(cl *Compiler, form *sexp.Subslice) {
	switch form.Kind() {
	case sexp.SpanLowOnly:
		call(cl, "Go--subslice-low", form.Slice, form.Low)
	case sexp.SpanHighOnly:
		call(cl, "Go--subslice-high", form.Slice, form.High)
	case sexp.SpanBoth:
		call(cl, "Go--subslice2", form.Slice, form.Low, form.High)
	case sexp.SpanWhole:
		/* Do nothing */
	}
}

func compileSubstr(cl *Compiler, form *sexp.Substr) {
	compileExpr(cl, form.Str)
	if form.Low == nil {
		emit(cl, instr.ConstRef(cl.cvec.InsertSym("nil")))
	} else {
		compileExpr(cl, form.Low)
	}
	if form.High == nil {
		emit(cl, instr.ConstRef(cl.cvec.InsertSym("nil")))
	} else {
		compileExpr(cl, form.High)
	}
	emit(cl, instr.Substr)
}

func compileStrCast(cl *Compiler, form *sexp.UnaryOp) {
	// []byte to string conversion.
	panic("unimplemented")
}

func compileArraySlice(cl *Compiler, form *sexp.ArraySlice) {
	switch form.Kind() {
	case sexp.SpanLowOnly:
		call(cl, "Go--array-slice-low", form.Array, form.Low)
	case sexp.SpanHighOnly:
		call(cl, "Go--array-slice-high", form.Array, form.High)
	case sexp.SpanBoth:
		call(cl, "Go--array-slice", form.Array, form.Low, form.High)
	case sexp.SpanWhole:
		call(cl, "Go--array-slice-whole", form.Array)
	}
}
