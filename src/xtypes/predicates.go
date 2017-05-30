package xtypes

import (
	"go/types"
)

// IsArray returns true when given argument underlying type is *types.Array.
func IsArray(typ types.Type) bool {
	_, ok := typ.Underlying().(*types.Array)
	return ok
}

// IsGlobal checks if given object belongs to a global scope.
func IsGlobal(obj types.Object) bool {
	objScope := obj.Parent()
	// If parent scope is Universe, then object scope
	// is Package => it is global.
	return objScope.Parent() == types.Universe
}
