package sexp

import (
	"go/types"
	"lisp/function"
)

// Constructors for simple forms.

func NewCall(fn *function.Type, args ...Form) *Call {
	return &Call{Fn: fn, Args: args}
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

func NewNot(arg Form) *UnaryOp     { return newOp1(OpNot, arg) }
func NewNeg(arg Form) *UnaryOp     { return newOp1(OpNeg, arg) }
func NewAdd1(arg Form) *UnaryOp    { return newOp1(OpAdd1, arg) }
func NewAdd2(arg Form) *UnaryOp    { return newOp1(OpAdd1, arg) }
func NewSub1(arg Form) *UnaryOp    { return newOp1(OpSub1, arg) }
func NewSub2(arg Form) *UnaryOp    { return newOp1(OpSub2, arg) }
func NewStrCast(arg Form) *UnaryOp { return newOp1(OpStrCast, arg) }

func NewShl(x, y Form) *BinOp    { return newOp2(OpShl, x, y) }
func NewShr(x, y Form) *BinOp    { return newOp2(OpShr, x, y) }
func NewBitOr(x, y Form) *BinOp  { return newOp2(OpBitOr, x, y) }
func NewBitAnd(x, y Form) *BinOp { return newOp2(OpBitAnd, x, y) }
func NewBitXor(x, y Form) *BinOp { return newOp2(OpBitXor, x, y) }
func NewAdd(x, y Form) *BinOp    { return newOp2(OpAdd, x, y) }
func NewSub(x, y Form) *BinOp    { return newOp2(OpSub, x, y) }
func NewMul(x, y Form) *BinOp    { return newOp2(OpMul, x, y) }
func NewQuo(x, y Form) *BinOp    { return newOp2(OpQuo, x, y) }
func NewNumEq(x, y Form) *BinOp  { return newOp2(OpNumEq, x, y) }
func NewNumNeq(x, y Form) *BinOp { return newOp2(OpNumNeq, x, y) }
func NewNumLt(x, y Form) *BinOp  { return newOp2(OpNumLt, x, y) }
func NewNumLte(x, y Form) *BinOp { return newOp2(OpNumLte, x, y) }
func NewNumGt(x, y Form) *BinOp  { return newOp2(OpNumGt, x, y) }
func NewNumGte(x, y Form) *BinOp { return newOp2(OpNumGte, x, y) }
func NewStrEq(x, y Form) *BinOp  { return newOp2(OpStrEq, x, y) }
func NewStrNeq(x, y Form) *BinOp { return newOp2(OpStrNeq, x, y) }
func NewStrLt(x, y Form) *BinOp  { return newOp2(OpStrLt, x, y) }
func NewStrLte(x, y Form) *BinOp { return newOp2(OpStrLte, x, y) }
func NewStrGt(x, y Form) *BinOp  { return newOp2(OpStrGt, x, y) }
func NewStrGte(x, y Form) *BinOp { return newOp2(OpStrGte, x, y) }
func NewConcat(x, y Form) *BinOp { return newOp2(OpConcat, x, y) }

func newOp2(kind OpKind, x, y Form) *BinOp {
	return &BinOp{Kind: kind, Args: [2]Form{x, y}}
}
func newOp1(kind OpKind, x Form) *UnaryOp {
	return &UnaryOp{Kind: kind, X: x}
}
