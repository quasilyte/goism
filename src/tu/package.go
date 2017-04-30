package tu

import (
	"go/token"
	"sexp"
)

// Package contains information about parsed code.
type Package struct {
	Name  string
	Funcs []*Func
}

// Func is a Sexp function.
type Func struct {
	Name   string
	Body   []sexp.Node
	Params []string
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
