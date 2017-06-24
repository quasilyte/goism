package lisp

import (
	"go/types"
)

var Package *types.Package

var (
	TypObject *types.Named
	TypSymbol *types.Named
)

func InitPackage(pkg *types.Package) error {
	top := pkg.Scope()
	getNamed := func(name string) *types.Named {
		return top.Lookup(name).(*types.TypeName).Type().(*types.Named)
	}

	Package = pkg

	TypObject = getNamed("Object")
	TypSymbol = getNamed("Symbol")

	return initFuncs()
}
