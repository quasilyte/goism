package sexp

import (
	"go/types"
	"magic_pkg/emacs/lisp"
)

// Constructors for simple forms.

func NewCall(fn *Func, args ...Form) *Call {
	return &Call{Fn: fn, Args: args}
}

func NewLispCall(fn *lisp.Func, args ...Form) *LispCall {
	return &LispCall{Fn: fn, Args: args}
}

func NewSubslice(slice, low, high Form) *SliceSlice {
	return &SliceSlice{
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

func NewSubstr(array, low, high Form) *LispCall {
	return &LispCall{Fn: lisp.FnSubstr, Args: []Form{array, low, high}}
}

func NewNot(x Form) *LispCall  { return NewLispCall(lisp.FnNot, x) }
func NewNeg(x Form) *LispCall  { return NewLispCall(lisp.FnNeg, x) }
func NewAdd1(x Form) *LispCall { return NewLispCall(lisp.FnAdd1, x) }
func NewSub1(x Form) *LispCall { return NewLispCall(lisp.FnSub1, x) }

func NewShl(x, y Form) *LispCall    { return NewLispCall(lisp.FnLsh, x, y) }
func NewShr(x, y Form) *LispCall    { return NewLispCall(lisp.FnLsh, x, NewNeg(y)) }
func NewBitOr(x, y Form) *LispCall  { return NewLispCall(lisp.FnLogior, x, y) }
func NewBitAnd(x, y Form) *LispCall { return NewLispCall(lisp.FnLogand, x, y) }
func NewBitXor(x, y Form) *LispCall { return NewLispCall(lisp.FnLogxor, x, y) }
func NewAdd(x, y Form) *LispCall    { return NewLispCall(lisp.FnAdd, x, y) }
func NewSub(x, y Form) *LispCall    { return NewLispCall(lisp.FnSub, x, y) }
func NewMul(x, y Form) *LispCall    { return NewLispCall(lisp.FnMul, x, y) }
func NewQuo(x, y Form) *LispCall    { return NewLispCall(lisp.FnQuo, x, y) }
func NewNumEq(x, y Form) *LispCall  { return NewLispCall(lisp.FnNumEq, x, y) }
func NewNumNeq(x, y Form) *LispCall { return NewNot(NewNumEq(x, y)) }
func NewNumLt(x, y Form) *LispCall  { return NewLispCall(lisp.FnNumLt, x, y) }
func NewNumLte(x, y Form) *LispCall { return NewLispCall(lisp.FnNumLte, x, y) }
func NewNumGt(x, y Form) *LispCall  { return NewLispCall(lisp.FnNumGt, x, y) }
func NewNumGte(x, y Form) *LispCall { return NewLispCall(lisp.FnNumGte, x, y) }
func NewStrEq(x, y Form) *LispCall  { return NewLispCall(lisp.FnStrEq, x, y) }
func NewStrNeq(x, y Form) *LispCall { return NewNot(NewStrEq(x, y)) }
func NewStrLt(x, y Form) *LispCall  { return NewLispCall(lisp.FnStrLt, x, y) }
func NewStrGt(x, y Form) *LispCall  { return NewLispCall(lisp.FnStrGt, x, y) }
func NewConcat(x, y Form) *LispCall { return NewLispCall(lisp.FnConcat, x, y) }
