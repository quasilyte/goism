package sexp

import (
	"go/types"
	"lisp"
)

var (
	typBool   = types.Typ[types.Bool]
	typInt    = types.Typ[types.Int64]
	typFloat  = types.Typ[types.Float64]
	typString = types.Typ[types.String]
	typVoid   = types.Typ[types.Invalid]
)

func (atom Bool) Type() types.Type {
	return typBool
}
func (atom Int) Type() types.Type {
	return typInt
}
func (atom Float) Type() types.Type {
	return typFloat
}
func (atom String) Type() types.Type {
	return typString
}
func (atom Symbol) Type() types.Type {
	return lisp.Types.Symbol
}

func (lit *ArrayLit) Type() types.Type {
	return lit.Typ
}
func (lit *SparseArrayLit) Type() types.Type {
	return lit.Typ
}

func (form *ArrayIndex) Type() types.Type {
	return form.Array.Type().Underlying().(*types.Array).Elem()
}
func (form *ArrayUpdate) Type() types.Type {
	return typVoid
}
func (form *ArrayCopy) Type() types.Type {
	return form.Array.Type()
}
func (form *SliceLen) Type() types.Type {
	return typInt
}
func (form *SliceCap) Type() types.Type {
	return typInt
}
func (form *SliceIndex) Type() types.Type {
	return form.Slice.Type().Underlying().(*types.Slice).Elem()
}
func (form *SliceUpdate) Type() types.Type {
	return typVoid
}

func (form *Panic) Type() types.Type {
	return typVoid
}
func (form *Bind) Type() types.Type {
	return typVoid
}
func (form *Rebind) Type() types.Type {
	return typVoid
}

func (form *TypeAssert) Type() types.Type {
	return form.Typ
}
func (form *LispTypeAssert) Type() types.Type {
	return form.Typ
}
func (form *FormList) Type() types.Type {
	return typVoid
}
func (form *Block) Type() types.Type {
	return typVoid
}
func (form *If) Type() types.Type {
	return typVoid
}
func (form *Return) Type() types.Type {
	return typVoid
}
func (form *While) Type() types.Type {
	return typVoid
}

func (op *NumAddX) Type() types.Type {
	return op.Typ
}
func (op *NumSubX) Type() types.Type {
	return op.Typ
}
func (op *NumAdd) Type() types.Type {
	return op.Typ
}
func (op *NumSub) Type() types.Type {
	return op.Typ
}
func (op *NumMul) Type() types.Type {
	return op.Typ
}
func (op *NumQuo) Type() types.Type {
	return op.Typ
}
func (op *NumEq) Type() types.Type {
	return op.Typ
}
func (op *NumNotEq) Type() types.Type {
	return op.Typ
}
func (op *NumLt) Type() types.Type {
	return op.Typ
}
func (op *NumLte) Type() types.Type {
	return op.Typ
}
func (op *NumGt) Type() types.Type {
	return op.Typ
}
func (op *NumGte) Type() types.Type {
	return op.Typ
}
func (op *Concat) Type() types.Type {
	return typString
}
func (op *StringEq) Type() types.Type {
	return typString
}
func (op *StringNotEq) Type() types.Type {
	return typString
}
func (op *StringLt) Type() types.Type {
	return typString
}
func (op *StringLte) Type() types.Type {
	return typString
}
func (op *StringGt) Type() types.Type {
	return typString
}
func (op *StringGte) Type() types.Type {
	return typString
}

func (call *Call) Type() types.Type {
	results := call.Fn.Results()
	if results.Len() == 1 {
		return results.At(0).Type()
	}
	return results
}
func (form CallStmt) Type() types.Type {
	return typVoid
}
func (form *MultiValueRef) Type() types.Type {
	return form.Typ
}
func (v Var) Type() types.Type {
	return v.Typ
}
