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

func (atom Bool) Type() types.Type   { return typBool }
func (atom Int) Type() types.Type    { return typInt }
func (atom Float) Type() types.Type  { return typFloat }
func (atom Str) Type() types.Type    { return typString }
func (atom Symbol) Type() types.Type { return lisp.Types.Symbol }

func (lit *ArrayLit) Type() types.Type       { return lit.Typ }
func (lit *SparseArrayLit) Type() types.Type { return lit.Typ }
func (lit *SliceLit) Type() types.Type       { return lit.Typ }

func (form *ArrayIndex) Type() types.Type {
	return form.Array.Type().Underlying().(*types.Array).Elem()
}
func (form *ArrayUpdate) Type() types.Type {
	return typVoid
}
func (form *ArrayCopy) Type() types.Type {
	return form.Array.Type()
}
func (form *ArraySlice) Type() types.Type {
	return form.Typ
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
func (form *Subslice) Type() types.Type {
	return form.Slice.Type()
}
func (form *Substr) Type() types.Type {
	return typString
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

func (form *FormList) Type() types.Type { return typVoid }
func (form *Block) Type() types.Type    { return typVoid }
func (form *If) Type() types.Type       { return typVoid }
func (form *Return) Type() types.Type   { return typVoid }
func (form *Repeat) Type() types.Type   { return typVoid }
func (form *DoTimes) Type() types.Type  { return typVoid }
func (form *While) Type() types.Type    { return typVoid }

func (op *UnaryOp) Type() types.Type { return op.X.Type() }
func (op *BinOp) Type() types.Type   { return op.Args[0].Type() }

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
func (form *Let) Type() types.Type {
	if form.Expr == nil {
		return typVoid
	}
	return form.Expr.Type()
}
