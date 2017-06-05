package sexp

import (
	"go/types"
	"magic_pkg/emacs/lisp"
	"xtypes"
)

func (atom Bool) Type() types.Type   { return xtypes.TypBool }
func (atom Int) Type() types.Type    { return xtypes.TypInt }
func (atom Float) Type() types.Type  { return xtypes.TypFloat }
func (atom Str) Type() types.Type    { return xtypes.TypString }
func (atom Symbol) Type() types.Type { return lisp.TypSymbol }
func (atom Var) Type() types.Type    { return atom.Typ }

func (lit *ArrayLit) Type() types.Type       { return lit.Typ }
func (lit *SparseArrayLit) Type() types.Type { return lit.Typ }
func (lit *SliceLit) Type() types.Type       { return lit.Typ }
func (form *StructLit) Type() types.Type     { return form.Typ }

func (form *ArrayUpdate) Type() types.Type  { return xtypes.TypVoid }
func (form *SliceUpdate) Type() types.Type  { return xtypes.TypVoid }
func (form *StructUpdate) Type() types.Type { return xtypes.TypVoid }
func (form *Bind) Type() types.Type         { return xtypes.TypVoid }
func (form *Rebind) Type() types.Type       { return xtypes.TypVoid }
func (form *VarUpdate) Type() types.Type    { return xtypes.TypVoid }
func (form *FormList) Type() types.Type     { return xtypes.TypVoid }
func (form *Block) Type() types.Type        { return xtypes.TypVoid }
func (form *If) Type() types.Type           { return xtypes.TypVoid }
func (form *Return) Type() types.Type       { return xtypes.TypVoid }
func (form *ExprStmt) Type() types.Type     { return xtypes.TypVoid }

func (form *Repeat) Type() types.Type  { return xtypes.TypVoid }
func (form *DoTimes) Type() types.Type { return xtypes.TypVoid }
func (form *While) Type() types.Type   { return xtypes.TypVoid }

func (form *ArrayIndex) Type() types.Type {
	return form.Array.Type().Underlying().(*types.Array).Elem()
}
func (form *SliceIndex) Type() types.Type {
	return form.Slice.Type().Underlying().(*types.Slice).Elem()
}
func (form *StructIndex) Type() types.Type {
	return form.Typ.Field(form.Index).Type()
}

func (form *ArraySlice) Type() types.Type { return form.Typ }
func (form *Subslice) Type() types.Type   { return form.Slice.Type() }
func (form *Substr) Type() types.Type     { return xtypes.TypString }

func (form *TypeAssert) Type() types.Type { return form.Typ }

func (call *Call) Type() types.Type {
	results := call.Fn.Results
	if results.Len() == 1 {
		return results.At(0).Type()
	}
	return results
}
func (call *LispCall) Type() types.Type {
	return call.Args[0].Type()
}
func (call *InstrCall) Type() types.Type {
	return lisp.TypObject
}
func (form *Let) Type() types.Type {
	if form.Expr == nil {
		return xtypes.TypVoid
	}
	return form.Expr.Type()
}

func (form *And) Type() types.Type { return xtypes.TypBool }
func (form *Or) Type() types.Type  { return xtypes.TypBool }
