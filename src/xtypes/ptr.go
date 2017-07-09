package xtypes

import (
	"go/types"
)

// Deref performs single pointer dereference.
// Returns pointer element type.
// Panics if given type is not a pointer.
func Deref(typ types.Type) types.Type {
	ptr := typ.(*types.Pointer)
	return ptr.Elem()
}

// MaybeDeref is like Deref, but returns passed argument on failure.
//
// Useful for types that are accessed in auto dereferencing manner which
// is limited to 1 level of indirection.
func MaybeDeref(typ types.Type) types.Type {
	if ptr, ok := typ.(*types.Pointer); ok {
		return ptr.Elem()
	}
	return typ
}
