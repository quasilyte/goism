package tu

import (
	"go/ast"
	"go/token"
	"go/types"
	"xast"
)

// Func holds function AST as well as additional information.
type Func struct {
	*xast.Tree
	Decl *ast.FuncDecl
}

// Package contains information about parsed code
// and type checked code.
type Package struct {
	Name string

	TypeInfo *types.Info

	Funcs []*Func

	FileSet *token.FileSet

	// Vars are sorted in order that should be used
	// during initialization.
	Vars []string
	// Package initializing expressions.
	Init []ast.Expr

	// All package comments.
	Comments []string
}

// TranslatePackage converts Go package into Sexp package.
//
// It parses and typechecks specified package,
// then converts generated objects into our format.
func TranslatePackage(pkgPath string) (*Package, error) {
	return translatePackage(pkgPath)
}
