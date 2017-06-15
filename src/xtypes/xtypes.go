package xtypes

import (
	"go/types"
)

var EmptyTuple = types.NewTuple()

var (
	TypBool   = types.Typ[types.Bool]
	TypInt    = types.Typ[types.Int64]
	TypFloat  = types.Typ[types.Float64]
	TypString = types.Typ[types.String]
	TypVoid   = types.Typ[types.Invalid]
)

func LookupField(name string, typ *types.Struct) int {
	for i := 0; i < typ.NumFields(); i++ {
		if typ.Field(i).Name() == name {
			return i
		}
	}
	return -1
}
