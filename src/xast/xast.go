package xast

import (
	"go/ast"
	"go/token"
	"go/types"
	"reflect"
)

// Package is type checked package which carries
// associated type info as well as parsed Go package.
type Package struct {
	AstPkg *ast.Package
	TypPkg *types.Package
	*types.Info
	FileSet  *token.FileSet
	FullName string
}

// Assign carries information that is needed to
// compile assign statement.
type Assign struct {
	Pkg *Package
	Lhs []*ast.Ident
	Rhs ast.Expr
}

// Func carries information that is needed to
// compile a function.
type Func struct {
	Pkg  *Package
	Ret  *types.Tuple
	Body *ast.BlockStmt
}

// ExprSlice converts any []T to []ast.Expr,
// where T is any type that implements ast.Expr.
func ExprSlice(nodes interface{}) []ast.Expr {
	s := reflect.ValueOf(nodes)
	res := make([]ast.Expr, s.Len())
	for i := range res {
		res[i] = s.Index(i).Interface().(ast.Expr)
	}
	return res
}
