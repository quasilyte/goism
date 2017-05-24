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
func (atom String) Type() types.Type { return typString }
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

func (op *Not) Type() types.Type  { return typBool }
func (op *Neg) Type() types.Type  { return op.Arg.Type() }
func (op *AddX) Type() types.Type { return op.Arg.Type() }
func (op *SubX) Type() types.Type { return op.Arg.Type() }

func (op *Shl) Type() types.Type      { return op.Args[0].Type() }
func (op *Shr) Type() types.Type      { return op.Args[0].Type() }
func (op *BitOr) Type() types.Type    { return op.Args[0].Type() }
func (op *BitAnd) Type() types.Type   { return op.Args[0].Type() }
func (op *BitXor) Type() types.Type   { return op.Args[0].Type() }
func (op *Add) Type() types.Type      { return op.Args[0].Type() }
func (op *Sub) Type() types.Type      { return op.Args[0].Type() }
func (op *Mul) Type() types.Type      { return op.Args[0].Type() }
func (op *Quo) Type() types.Type      { return op.Args[0].Type() }
func (op *NumEq) Type() types.Type    { return op.Args[0].Type() }
func (op *NumNotEq) Type() types.Type { return op.Args[0].Type() }
func (op *NumLt) Type() types.Type    { return op.Args[0].Type() }
func (op *NumLte) Type() types.Type   { return op.Args[0].Type() }
func (op *NumGt) Type() types.Type    { return op.Args[0].Type() }
func (op *NumGte) Type() types.Type   { return op.Args[0].Type() }

func (op *StrEq) Type() types.Type    { return typString }
func (op *StrNotEq) Type() types.Type { return typString }
func (op *StrLt) Type() types.Type    { return typString }
func (op *StrLte) Type() types.Type   { return typString }
func (op *StrGt) Type() types.Type    { return typString }
func (op *StrGte) Type() types.Type   { return typString }

func (op *Concat) Type() types.Type { return typString }

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
