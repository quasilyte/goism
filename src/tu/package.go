package tu

import (
	"go/token"
	"sexp"
)

// Package contains information about parsed code.
type Package struct {
	Name string

	Funcs []*Func

	// Vars are sorted in order that should be used
	// during initialization.
	Vars []*Var
}

// Func is a Sexp function.
type Func struct {
	Name   string
	Body   []sexp.Form
	Params []string
}

// Var is a package-level (global) variable.
type Var struct {
	Name string
	Init sexp.Form
}

// TranslatePackage converts Go package into Sexp package.
//
// It parses and typechecks specified package,
// then converts generated objects into our format.
func TranslatePackage(pkgPath string) (*Package, error) {
	fSet := token.NewFileSet()

	parsedPkg, err := parsePackage(fSet, pkgPath)
	if err != nil {
		return nil, err
	}

	checkedPkg, err := typecheckPackage(fSet, parsedPkg)
	if err != nil {
		return nil, err
	}

	return translatePackage(checkedPkg), nil
}
