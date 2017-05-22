package compiler

import (
	"fmt"
	"ir/instr"
	"lisp/function"
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
	case sexp.CallStmt:
		compileCallStmt(cl, form)
	case *sexp.Panic:
		compilePanic(cl, form)
	case *sexp.While:
		compileWhile(cl, form)
	case *sexp.ArrayUpdate:
		compileArrayUpdate(cl, form)
	case *sexp.SliceUpdate:
		compileSliceUpdate(cl, form)

	default:
		panic(fmt.Sprintf("unexpected stmt: %#v\n", form))
	}
}

func compileExpr(cl *Compiler, form sexp.Form) {
	switch form := form.(type) {
	case sexp.Int:
		emit(cl, instr.ConstRef(cl.cvec.InsertInt(form.Val)))
	case sexp.Float:
		emit(cl, instr.ConstRef(cl.cvec.InsertFloat(form.Val)))
	case sexp.String:
		emit(cl, instr.ConstRef(cl.cvec.InsertString(form.Val)))
	case sexp.Symbol:
		emit(cl, instr.ConstRef(cl.cvec.InsertSym(form.Val)))
	case sexp.Bool:
		compileBool(cl, form)
	case sexp.Var:
		compileVar(cl, form)

	case *sexp.ArrayLit:
		call(cl, function.Vector, form.Vals...)
	case *sexp.SparseArrayLit:
		compileSparseArrayLit(cl, form)

	case *sexp.ArrayIndex:
		compileArrayIndex(cl, form)
	case *sexp.ArrayCopy:
		compileArrayCopy(cl, form)
	case *sexp.SliceIndex:
		compileSliceIndex(cl, form)

	case *sexp.NumAddX:
		compileUnaryOps(cl, instr.Add1, form.Arg, form.X)
	case *sexp.NumSubX:
		compileUnaryOps(cl, instr.Sub1, form.Arg, form.X)
	case *sexp.NumAdd:
		compileBinOp(cl, instr.NumAdd, form.Args)
	case *sexp.NumSub:
		compileBinOp(cl, instr.NumSub, form.Args)
	case *sexp.NumMul:
		compileBinOp(cl, instr.NumMul, form.Args)
	case *sexp.NumQuo:
		compileBinOp(cl, instr.NumQuo, form.Args)
	case *sexp.NumGt:
		compileBinOp(cl, instr.NumGt, form.Args)
	case *sexp.NumLt:
		compileBinOp(cl, instr.NumLt, form.Args)
	case *sexp.NumEq:
		compileBinOp(cl, instr.NumEq, form.Args)

	case *sexp.Concat:
		compileVariadicOp(cl, instr.Concat(len(form.Args)), form.Args)

	case *sexp.Call:
		compileCall(cl, form)
	case *sexp.MultiValueRef:
		compileMultiValueRef(cl, form)

	default:
		panic(fmt.Sprintf("unexpected expr: %#v\n", form))
	}
}
