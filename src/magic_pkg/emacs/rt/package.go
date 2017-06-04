package rt

import (
	"go/types"
)

var Package *types.Package

var (
	TypSlice *types.Struct
)

func InitPackage(pkg *types.Package) {
	top := pkg.Scope()
	getStruct := func(name string) *types.Struct {
		tn := top.Lookup(name).(*types.TypeName)
		return tn.Type().(*types.Named).Underlying().(*types.Struct)
	}

	Package = pkg

	TypSlice = getStruct("Slice")
}
