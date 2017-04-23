package sexp

import (
	"go/types"
)

var (
	boolType    = types.Typ[types.Bool]
	intType     = types.Typ[types.Int64]
	floatType   = types.Typ[types.Float64]
	stringType  = types.Typ[types.String]
	invalidType = types.Typ[types.Invalid]
)

func (atom Bool) Type() types.Type   { return boolType }
func (atom Int) Type() types.Type    { return intType }
func (atom Float) Type() types.Type  { return floatType }
func (atom String) Type() types.Type { return stringType }
func (atom Var) Type() types.Type    { return atom.Typ }

func (lit *ArrayLit) Type() types.Type    { return lit.Typ }
func (lit *QuotedArray) Type() types.Type { return lit.Typ }

func (form *Block) Type() types.Type  { return invalidType }
func (form *If) Type() types.Type     { return invalidType }
func (form *Bind) Type() types.Type   { return invalidType }
func (form *Return) Type() types.Type { return form.Result.Type() }

func (op *Operation) Type() types.Type { return op.Typ }

func (call *Call) Type() types.Type { return call.Typ }
