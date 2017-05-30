package compiler

import (
	"exn"
	"ir/instr"
	"sexp"
)

func compileStmt(cl *Compiler, form sexp.Form) {
	switch form := form.(type) {
	case *sexp.Return:
		compileReturn(cl, form)
	case *sexp.If:
		compileIf(cl, form)
	case *sexp.Block:
		compileBlock(cl, form)
	case *sexp.FormList:
		compileStmtList(cl, form.Forms)
	case *sexp.Bind:
		compileBind(cl, form)
	case *sexp.Rebind:
		compileRebind(cl, form)
	case *sexp.VarUpdate:
		compileVarUpdate(cl, form)
	case sexp.CallStmt:
		compileCallStmt(cl, form)
	case *sexp.Panic:
		call(cl, "Go--panic", form.ErrorData)
	case *sexp.Repeat:
		compileRepeat(cl, form)
	case *sexp.While:
		compileWhile(cl, form)
	case *sexp.ArrayUpdate:
		compileArrayUpdate(cl, form)
	case *sexp.SliceUpdate:
		compileSliceUpdate(cl, form)

	case *sexp.Let:
		compileLetStmt(cl, form)

	default:
		panic(exn.Logic("unexpected stmt: %#v", form))
	}
}

func compileExpr(cl *Compiler, form sexp.Form) {
	switch form := form.(type) {
	case sexp.Int:
		emit(cl, instr.ConstRef(cl.cvec.InsertInt(int64(form))))
	case sexp.Float:
		emit(cl, instr.ConstRef(cl.cvec.InsertFloat(float64(form))))
	case sexp.Str:
		emit(cl, instr.ConstRef(cl.cvec.InsertString(string(form))))
	case sexp.Symbol:
		emit(cl, instr.ConstRef(cl.cvec.InsertSym(form.Val)))
	case sexp.Bool:
		compileBool(cl, form)
	case sexp.Var:
		compileVar(cl, form)

	case *sexp.ArrayLit:
		call(cl, "vector", form.Vals...)
	case *sexp.SparseArrayLit:
		compileSparseArrayLit(cl, form)
	case *sexp.SliceLit:
		call(cl, "Go--make-slice-from-list", form.Vals...)

	case *sexp.ArrayIndex:
		compileArrayIndex(cl, form)
	case *sexp.ArraySlice:
		compileArraySlice(cl, form)

	case *sexp.SliceIndex:
		compileSliceIndex(cl, form)
	case *sexp.Subslice:
		compileSubslice(cl, form)

	case *sexp.Substr:
		compileSubstr(cl, form)

	case *sexp.BinOp:
		compileBinOp(cl, form)

	case *sexp.UnaryOp:
		compileUnaryOp(cl, form)

	case *sexp.Call:
		compileCall(cl, form.Fn.Name(), form.Args)

	case *sexp.Let:
		compileLetExpr(cl, form)

	default:
		panic(exn.Logic("unexpected expr: %#v", form))
	}
}
