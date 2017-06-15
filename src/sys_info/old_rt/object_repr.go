package old_rt

import (
	"exn"
	"go/types"
)

type StructRepr int

const (
	StructEmpty StructRepr = iota
	StructUnit
	StructCons
	StructVec
)

func StructReprOf(typ *types.Struct) StructRepr {
	switch typ.NumFields() {
	case 0:
		panic(exn.NoImpl("empty structs"))
		return StructEmpty
	case 1:
		return StructUnit
	case 2, 3, 4:
		return StructCons
	default:
		return StructVec
	}
}
