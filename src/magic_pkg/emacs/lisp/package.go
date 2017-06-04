package lisp

import (
	"go/types"
)

var Package *types.Package

var (
	TypObject *types.Interface
	TypInt    *types.Named
	TypFloat  *types.Named
	TypStr    *types.Named
	TypSymbol *types.Named
	TypBool   *types.Named
)

func InitPackage(pkg *types.Package) {
	top := pkg.Scope()
	getNamed := func(name string) *types.Named {
		return top.Lookup(name).(*types.TypeName).Type().(*types.Named)
	}

	Package = pkg

	TypInt = getNamed("Int")
	TypFloat = getNamed("Float")
	TypStr = getNamed("Str")
	TypSymbol = getNamed("Symbol")
	TypBool = getNamed("Bool")
	TypObject = top.Lookup("Object").Type().Underlying().(*types.Interface)
}
