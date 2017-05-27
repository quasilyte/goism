package compiler

import (
	"fmt"
	"ir/instr"
	"lisp/function"
	"sexp"
)

func tryCompileStmt(cl *Compiler, form sexp.Form) bool {
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
		return false
	}

	return true
}

func tryCompileExpr(cl *Compiler, form sexp.Form) bool {
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
		call(cl, function.Vector, form.Vals...)
	case *sexp.SparseArrayLit:
		compileSparseArrayLit(cl, form)
	case *sexp.SliceLit:
		call(cl, function.MakeSliceFromList(form.Typ), form.Vals...)

	case *sexp.ArrayIndex:
		compileArrayIndex(cl, form)
	case *sexp.ArrayCopy:
		compileArrayCopy(cl, form)
	case *sexp.ArraySlice:
		compileArraySlice(cl, form)

	case *sexp.SliceIndex:
		compileSliceIndex(cl, form)
	case *sexp.SliceLen:
		compileSliceLen(cl, form)
	case *sexp.SliceCap:
		compileSliceCap(cl, form)
	case *sexp.Subslice:
		compileSubslice(cl, form)

	case *sexp.Substr:
		compileSubstr(cl, form)

	case *sexp.BinOp:
		switch form.Kind {
		case sexp.OpShl:
			call(cl, function.Lsh, form.Args[0], form.Args[1])
		case sexp.OpBitAnd:
			call(cl, function.Logand, form.Args[0], form.Args[1])
		case sexp.OpBitOr:
			call(cl, function.Logior, form.Args[0], form.Args[1])
		case sexp.OpAdd:
			compileBinOp(cl, instr.NumAdd, form.Args)
		case sexp.OpSub:
			compileBinOp(cl, instr.NumSub, form.Args)
		case sexp.OpMul:
			compileBinOp(cl, instr.NumMul, form.Args)
		case sexp.OpQuo:
			compileBinOp(cl, instr.NumQuo, form.Args)
		case sexp.OpNumGt:
			compileBinOp(cl, instr.NumGt, form.Args)
		case sexp.OpNumLt:
			compileBinOp(cl, instr.NumLt, form.Args)
		case sexp.OpNumEq:
			compileBinOp(cl, instr.NumEq, form.Args)
		case sexp.OpConcat:
			compileBinOp(cl, instr.Concat(2), form.Args)
		}

	case *sexp.UnaryOp:
		switch form.Kind {
		case sexp.OpNot:
			compileUnaryOp(cl, instr.Not, form.X)
		case sexp.OpNeg:
			compileUnaryOp(cl, instr.Neg, form.X)
		case sexp.OpStrCast:
			compileStrCast(cl, form)

		case sexp.OpSub1:
			compileUnaryOp(cl, instr.Sub1, form.X)
		case sexp.OpSub2:
			compileUnaryOp(cl, instr.Sub1, form.X)
			compileUnaryOp(cl, instr.Sub1, form.X)
		case sexp.OpAdd1:
			compileUnaryOp(cl, instr.Add1, form.X)
		case sexp.OpAdd2:
			compileUnaryOp(cl, instr.Add1, form.X)
			compileUnaryOp(cl, instr.Add1, form.X)
		}

	case *sexp.Call:
		compileCall(cl, form)
	case *sexp.MultiValueRef:
		compileMultiValueRef(cl, form)

	case *sexp.Let:
		compileLetExpr(cl, form)

	default:
		return false
	}

	return true
}

func compileStmt(cl *Compiler, form sexp.Form) {
	if !tryCompileStmt(cl, form) {
		panic(fmt.Sprintf("unexpected stmt: %#v\n", form))
	}
}

func compileExpr(cl *Compiler, form sexp.Form) {
	if !tryCompileExpr(cl, form) {
		panic(fmt.Sprintf("unexpected expr: %#v\n", form))
	}
}
