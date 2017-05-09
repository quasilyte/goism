package lisp

import (
	"go/types"
)

type Symbol string

var Package *types.Package

var Typenames struct {
	Int    *types.TypeName
	Float  *types.TypeName
	String *types.TypeName
	Symbol *types.TypeName
	Bool   *types.TypeName
}

var Types struct {
	Object *types.Interface
	Int    *types.Named
	Float  *types.Named
	String *types.Named
	Symbol *types.Named
	Bool   *types.Named
}

func InitPackage(pkg *types.Package) {
	top := pkg.Scope()
	getTypename := func(name string) *types.TypeName {
		return top.Lookup(name).(*types.TypeName)
	}
	getNamed := func(tn *types.TypeName) *types.Named {
		return tn.Type().(*types.Named)
	}

	Package = pkg

	Typenames.Int = getTypename("Int")
	Types.Int = getNamed(Typenames.Int)
	Typenames.Float = getTypename("Float")
	Types.Float = getNamed(Typenames.Float)
	Typenames.String = getTypename("String")
	Types.String = getNamed(Typenames.String)
	Typenames.Symbol = getTypename("Symbol")
	Types.Symbol = getNamed(Typenames.Symbol)
	Typenames.Bool = getTypename("Bool")
	Types.Bool = getNamed(Typenames.Bool)

	Types.Object = top.Lookup("Object").Type().Underlying().(*types.Interface)
}
