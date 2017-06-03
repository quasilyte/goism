package load

import "tu"

// Package converts Go package into Sexp package.
//
// It parses and typechecks specified package,
// then converts generated objects into our format.
func Package(pkgPath string) (pkg *tu.Package, err error) {
	return translatePackage(pkgPath)
}
