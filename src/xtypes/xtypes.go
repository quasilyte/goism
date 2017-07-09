package xtypes

import (
	"go/types"
)

var EmptyTuple = types.NewTuple()

// Convenient common types accessors.
var (
	TypBool   = types.Typ[types.Bool]
	TypInt    = types.Typ[types.Int64]
	TypFloat  = types.Typ[types.Float64]
	TypString = types.Typ[types.String]
	TypVoid   = types.Typ[types.Invalid]
)

// AsNamedType tries to convert given type to "types.Named".
// Returns nil if not possible.
// Can perform single pointer dereference if needed.
func AsNamedType(typ types.Type) *types.Named {
	if typ, ok := MaybeDeref(typ).(*types.Named); ok {
		return typ
	}
	return nil
}

// LookupField find struct field position (index).
// Returns -1 on lookup failure.
func LookupField(name string, typ *types.Struct) int {
	for i := 0; i < typ.NumFields(); i++ {
		if typ.Field(i).Name() == name {
			return i
		}
	}
	return -1
}

// LookupIfaceMethod find interface method position (index).
// Returns -1 on lookup failure.
func LookupIfaceMethod(name string, typ *types.Interface) int {
	for i := 0; i < typ.NumMethods(); i++ {
		if typ.Method(i).Name() == name {
			return i
		}
	}
	return -1
}
