package tu

import (
	"sexp"
)

// Package contains information about parsed code.
type Package struct {
	Name string

	Funcs []*Func

	// Vars are sorted in order that should be used
	// during initialization.
	Vars []string
	Init *Func

	Comment string
}

// Func is a Sexp function.
type Func struct {
	Name      string
	Body      *sexp.Block
	Params    []string
	Variadic  bool
	DocString string
}

// TranslatePackage converts Go package into Sexp package.
//
// It parses and typechecks specified package,
// then converts generated objects into our format.
func TranslatePackage(pkgPath string) (pkg *Package, err error) {
	return translatePackage(pkgPath)
}
