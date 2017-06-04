package xtypes

import (
	"go/types"
)

func LookupField(name string, typ *types.Struct) int {
	for i := 0; i < typ.NumFields(); i++ {
		if typ.Field(i).Name() == name {
			return i
		}
	}
	return -1
}
