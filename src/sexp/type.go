package sexp

import (
	"go/types"
	"magic_pkg/emacs/lisp"
	"xtypes"
)

func (atom Bool) Type() types.Type   { return xtypes.TypBool }
func (atom Int) Type() types.Type    { return xtypes.TypInt }
func (atom Float) Type() types.Type  { return xtypes.TypFloat64 }
func (atom Str) Type() types.Type    { return xtypes.TypString }
func (atom Symbol) Type() types.Type { return lisp.TypSymbol }
func (atom Var) Type() types.Type    { return atom.Typ }
func (atom Local) Type() types.Type  { return atom.Typ }

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
func (form FormList) Type() types.Type      { return xtypes.TypVoid }
func (form Block) Type() types.Type         { return xtypes.TypVoid }
func (form *If) Type() types.Type           { return xtypes.TypVoid }
func (form *Switch) Type() types.Type       { return xtypes.TypVoid }
func (form *SwitchTrue) Type() types.Type   { return xtypes.TypVoid }
func (form *Return) Type() types.Type       { return xtypes.TypVoid }
func (form *ExprStmt) Type() types.Type     { return xtypes.TypVoid }
func (form *Goto) Type() types.Type         { return xtypes.TypVoid }
func (form *Label) Type() types.Type        { return xtypes.TypVoid }

func (form *Repeat) Type() types.Type  { return xtypes.TypVoid }
func (form *DoTimes) Type() types.Type { return xtypes.TypVoid }
func (form *Loop) Type() types.Type    { return xtypes.TypVoid }
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
func (form *SliceSlice) Type() types.Type { return form.Slice.Type() }

func (form *TypeAssert) Type() types.Type { return form.Typ }

func (call *Call) Type() types.Type {
	results := call.Fn.Results
	if results.Len() == 1 {
		return results.At(0).Type()
	}
	return results
}
func (call *LispCall) Type() types.Type {
	switch call.Fn {
	case lisp.FnSub, lisp.FnAdd, lisp.FnMul, lisp.FnQuo, lisp.FnMin:
		return call.Args[0].Type()

	case lisp.FnConcat:
		return xtypes.TypString

	case lisp.FnLen:
		return xtypes.TypInt

	case lisp.FnEq, lisp.FnEqual, lisp.FnNot:
		return xtypes.TypBool
	case lisp.FnNumEq, lisp.FnNumLt, lisp.FnNumGt, lisp.FnNumLte, lisp.FnNumGte:
		return xtypes.TypBool
	case lisp.FnStrEq, lisp.FnStrLt, lisp.FnStrGt:
		return xtypes.TypBool
	case lisp.FnIsInt, lisp.FnIsStr, lisp.FnIsSymbol:
		return xtypes.TypBool

	default:
		return lisp.TypObject
	}
}
func (call *LambdaCall) Type() types.Type { return call.Typ }
func (call *DynCall) Type() types.Type    { return call.Typ }

func (form *Let) Type() types.Type {
	if form.Expr == nil {
		return xtypes.TypVoid
	}
	return form.Expr.Type()
}
func (form *TypeCast) Type() types.Type {
	return form.Typ
}

func (form *And) Type() types.Type { return xtypes.TypBool }
func (form *Or) Type() types.Type  { return xtypes.TypBool }

func (form *emptyForm) Type() types.Type { return xtypes.TypVoid }
