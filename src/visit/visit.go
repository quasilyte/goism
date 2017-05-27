// Package visit provides more convenient access to specific AST parts.
// It operates on forms simplified by xast package.
package visit

import (
	"go/ast"
	"go/token"
)

func Bind(decl *ast.GenDecl, f func(*ast.Ident, ast.Expr)) bool {
	if decl.Tok == token.VAR {
		spec := decl.Specs[0].(*ast.ValueSpec)
		f(spec.Names[0], spec.Values[0])
		return true
	}
	return false
}

func Rebind(stmt *ast.AssignStmt, f func(*ast.Ident, ast.Expr)) bool {
	if lhs, ok := stmt.Lhs[0].(*ast.Ident); ok {
		f(lhs, stmt.Rhs[0])
		return true
	}
	return false
}
