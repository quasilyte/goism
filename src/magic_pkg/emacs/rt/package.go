package rt

import (
	"assert"
	"fmt"
	"go/constant"
	"go/types"
	"tu/symbols"
)

var Package *types.Package

var RetVars []string

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

	RetVars = make([]string, multiRetLimit(top))
	for i := 1; i < len(RetVars); i++ {
		name := fmt.Sprintf("Ret%d", i+1)
		assert.True(top.Lookup(name) != nil)
		RetVars[i] = symbols.Mangle(pkg.Name(), name)
	}

	TypSlice = getStruct("Slice")
}

func multiRetLimit(top *types.Scope) int {
	val, _ := constant.Int64Val(top.Lookup("MultiRetLimit").(*types.Const).Val())
	return int(val)
}
