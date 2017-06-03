package sexp

import (
	"go/types"
	"ir/instr"
	"lisp/function"
)

// Constructors for simple forms.

func NewCall(fn *Func, args ...Form) *Call {
	return &Call{Fn: fn, Args: args}
}

func NewLispCall(fn *function.LispFn, args ...Form) *LispCall {
	return &LispCall{Fn: fn, Args: args}
}

func NewSubslice(slice, low, high Form) *Subslice {
	return &Subslice{
		Slice: slice,
		Span:  Span{Low: low, High: high},
	}
}

func NewArraySlice(array, low, high Form) *ArraySlice {
	return &ArraySlice{
		Array: array,
		Typ:   types.NewSlice(array.Type().(*types.Array).Elem()),
		Span:  Span{Low: low, High: high},
	}
}

func NewSubstr(str, low, high Form) *Substr {
	return &Substr{
		Str:  str,
		Span: Span{Low: low, High: high},
	}
}

func NewNot(x Form) *InstrCall    { return newOp1(instr.Not, x) }
func NewNeg(x Form) *InstrCall    { return newOp1(instr.Neg, x) }
func NewAdd1(x Form) *InstrCall   { return newOp1(instr.Add1, x) }
func NewSub1(x Form) *InstrCall   { return newOp1(instr.Sub1, x) }
func NewStrCast(x Form) *LispCall { return NewLispCall(function.StrCast, x) }

func NewShl(x, y Form) *LispCall     { return NewLispCall(function.Lsh, x, y) }
func NewShr(x, y Form) *LispCall     { return NewLispCall(function.Lsh, x, NewNeg(y)) }
func NewBitOr(x, y Form) *LispCall   { return NewLispCall(function.Logior, x, y) }
func NewBitAnd(x, y Form) *LispCall  { return NewLispCall(function.Logand, x, y) }
func NewBitXor(x, y Form) *LispCall  { return NewLispCall(function.Logxor, x, y) }
func NewAdd(x, y Form) *InstrCall    { return newOp2(instr.NumAdd, x, y) }
func NewSub(x, y Form) *InstrCall    { return newOp2(instr.NumSub, x, y) }
func NewMul(x, y Form) *InstrCall    { return newOp2(instr.NumMul, x, y) }
func NewQuo(x, y Form) *InstrCall    { return newOp2(instr.NumQuo, x, y) }
func NewNumEq(x, y Form) *InstrCall  { return newOp2(instr.NumEq, x, y) }
func NewNumNeq(x, y Form) *InstrCall { return NewNot(NewNumEq(x, y)) }
func NewNumLt(x, y Form) *InstrCall  { return newOp2(instr.NumLt, x, y) }
func NewNumLte(x, y Form) *InstrCall { return newOp2(instr.NumLte, x, y) }
func NewNumGt(x, y Form) *InstrCall  { return newOp2(instr.NumGt, x, y) }
func NewNumGte(x, y Form) *InstrCall { return newOp2(instr.NumGte, x, y) }
func NewStrEq(x, y Form) *InstrCall  { return newOp2(instr.StrEq, x, y) }
func NewStrNeq(x, y Form) *InstrCall { return NewNot(NewStrEq(x, y)) }
func NewStrLt(x, y Form) *InstrCall  { return newOp2(instr.StrLt, x, y) }
func NewStrGt(x, y Form) *LispCall   { return NewLispCall(function.StrGt, x, y) }
func NewConcat(x, y Form) *InstrCall { return newOp2(instr.Concat2, x, y) }

func newOp1(ins instr.Instr, x Form) *InstrCall {
	return &InstrCall{Instr: ins, Args: []Form{x}}
}
func newOp2(ins instr.Instr, x, y Form) *InstrCall {
	return &InstrCall{Instr: ins, Args: []Form{x, y}}
}
