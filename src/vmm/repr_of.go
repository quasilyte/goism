package vmm

import (
	"exn"
	"go/types"
)

// StructRepr specifies runtime object representation (data layout).
type StructRepr int

// When do we give up lists and use vectors.
const consReprThreshold = 4

const (
	// StructEmpty - zero size object.
	StructEmpty StructRepr = iota
	// StructUnit - single attribute struct.
	// Represented as (cons X nil).
	StructUnit
	// StructCons - struct of [2, consReprThreshold] attributes.
	// Represented as improper list (chained conses).
	StructCons
	// StructVec - struct which has more that consReprThreshold+1 attributes.
	// Represented as vector.
	StructVec
)

// StructReprOf returns enumeration that describes object representation.
func StructReprOf(typ *types.Struct) StructRepr {
	fields := typ.NumFields()
	switch {
	case fields == 0:
		panic(exn.NoImpl("empty structs"))
		return StructEmpty
	case fields == 1:
		return StructUnit
	case fields <= consReprThreshold:
		return StructCons
	default:
		return StructVec
	}
}
