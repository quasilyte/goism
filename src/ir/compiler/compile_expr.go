package compiler

import (
	"ir/instr"
	"sexp"
)

var opKindToInstr = [...]instr.Instr{
	sexp.OpAdd:    instr.NumAdd,
	sexp.OpSub:    instr.NumSub,
	sexp.OpMul:    instr.NumMul,
	sexp.OpQuo:    instr.NumQuo,
	sexp.OpNumGt:  instr.NumGt,
	sexp.OpNumLt:  instr.NumLt,
	sexp.OpNumEq:  instr.NumEq,
	sexp.OpConcat: instr.Concat2,
	sexp.OpNot:    instr.Not,
	sexp.OpNeg:    instr.Neg,
	sexp.OpAdd1:   instr.Add1,
	sexp.OpSub1:   instr.Sub1,
	sexp.OpAdd2:   instr.Add1,
	sexp.OpSub2:   instr.Sub1,
}

func compileBinOp(cl *Compiler, form *sexp.BinOp) {
	switch form.Kind {
	case sexp.OpShl:
		call(cl, "lsh", form.Args[0], form.Args[1])
	case sexp.OpBitAnd:
		call(cl, "logand", form.Args[0], form.Args[1])
	case sexp.OpBitOr:
		call(cl, "logior", form.Args[0], form.Args[1])

	default:
		compileExpr(cl, form.Args[0])
		compileExpr(cl, form.Args[1])
		emit(cl, opKindToInstr[form.Kind])
	}
}

func compileUnaryOp(cl *Compiler, form *sexp.UnaryOp) {
	switch form.Kind {
	case sexp.OpStrCast:
		compileStrCast(cl, form)
	case sexp.OpArrayCopy:
		call(cl, "copy-sequence", form.X)

	default:
		compileExpr(cl, form.X)
		emit(cl, opKindToInstr[form.Kind])
		if form.Kind == sexp.OpAdd2 || form.Kind == sexp.OpSub2 {
			emit(cl, opKindToInstr[form.Kind])
		}
	}
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

func compileArrayIndex(cl *Compiler, form *sexp.ArrayIndex) {
	compileExpr(cl, form.Array)
	compileExpr(cl, form.Index)
	emit(cl, instr.ArrayRef)
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
