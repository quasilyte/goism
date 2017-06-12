package compiler

import (
	"ir/instr"
	"sexp"
	"sys_info/old_rt"
)

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
	compileExpr(cl, form.Ctor)
	for i, val := range form.Vals {
		emit(cl, instr.StackRef(0)) // Array
		emit(cl, instr.ConstRef(cl.cvec.InsertInt(int64(i))))
		compileExpr(cl, val)
		emit(cl, instr.ArraySet)
	}
}

func compileCall(cl *Compiler, name string, args []sexp.Form) {
	emit(cl, instr.ConstRef(cl.cvec.InsertSym(name)))
	compileExprList(cl, args)
	emit(cl, instr.Call(len(args)))
}

func compileInstrCall(cl *Compiler, form *sexp.InstrCall) {
	compileExprList(cl, form.Args)
	emit(cl, form.Instr)
}

func compileArrayIndex(cl *Compiler, form *sexp.ArrayIndex) {
	compileExpr(cl, form.Array)
	compileExpr(cl, form.Index)
	emit(cl, instr.ArrayRef)
}

func compileLetExpr(cl *Compiler, form *sexp.Let) {
	for _, bind := range form.Bindings {
		compileBind(cl, bind)
	}
	compileExpr(cl, form.Expr)
	emit(cl, instr.StackSet(len(form.Bindings)))
	if len(form.Bindings) > 1 {
		emit(cl, instr.Discard(len(form.Bindings)-1))
	}
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

func compileStructLit(cl *Compiler, form *sexp.StructLit) {
	switch old_rt.StructReprOf(form.Typ) {
	case old_rt.StructAtom:
		compileExpr(cl, form.Vals[0])

	case old_rt.StructCons:
		compileExprList(cl, form.Vals)
		emitN(cl, instr.Cons, form.Typ.NumFields()-1)

	case old_rt.StructVec:
		call(cl, "vector", form.Vals...)
	}
}

func compileStructIndex(cl *Compiler, form *sexp.StructIndex) {
	switch old_rt.StructReprOf(form.Typ) {
	case old_rt.StructAtom:
		compileExpr(cl, form.Struct)

	case old_rt.StructCons:
		compileExpr(cl, form.Struct)
		emitN(cl, instr.Cdr, form.Index)
		if form.Typ.NumFields() != form.Index+1 { // Not last index.
			emit(cl, instr.Car)
		}

	case old_rt.StructVec:
		compileArrayIndex(cl, &sexp.ArrayIndex{
			Array: form.Struct,
			Index: sexp.Int(form.Index),
		})
	}
}

func compileAnd(cl *Compiler, form *sexp.And) {
	resLabel := labelCreate(cl, "and")
	compileExpr(cl, form.X)
	emitJmpNilElsePop(cl, resLabel)
	compileExpr(cl, form.Y)
	labelBind(cl, resLabel)
}

func compileOr(cl *Compiler, form *sexp.Or) {
	resLabel := labelCreate(cl, "or")
	compileExpr(cl, form.X)
	emitJmpNotNilElsePop(cl, resLabel)
	compileExpr(cl, form.Y)
	labelBind(cl, resLabel)
}
