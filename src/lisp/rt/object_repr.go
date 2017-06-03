package rt

import (
	"exn"
	"go/types"
)

type StructRepr int

const (
	StructEmpty StructRepr = iota
	StructAtom
	StructCons
	StructVec
)

func StructReprOf(typ *types.Struct) StructRepr {
	switch typ.NumFields() {
	case 0:
		panic(exn.NoImpl("empty structs"))
		return StructEmpty
	case 1:
		return StructAtom
	case 2, 3, 4:
		return StructCons
	default:
		return StructVec
	}
}
