package compiler

import (
	"exn"
	"go/types"
)

type structRepr int

const (
	structEmpty structRepr = iota
	structAtom
	structCons
	structVec
)

func structReprOf(typ *types.Struct) structRepr {
	switch typ.NumFields() {
	case 0:
		panic(exn.NoImpl("empty structs"))
		return structEmpty
	case 1:
		return structAtom
	case 2, 3, 4:
		return structCons
	default:
		return structVec
	}
}
