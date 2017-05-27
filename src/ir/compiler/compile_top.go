package compiler

import (
	"go/ast"
)

// Handling top level.

func (cl *Compiler) compileExpr(node ast.Expr) {
	switch node := node.(type) {
	case *ast.BasicLit:
		cl.compileBasicLit(node)
	case *ast.Ident:
		cl.compileIdent(node)
	case *ast.CallExpr:
		cl.compileCallExpr(node)
	case *ast.BinaryExpr:
		cl.compileBinaryExpr(node)
	case *ast.UnaryExpr:
		cl.compileUnaryExpr(node)
	case *ast.CompositeLit:
		cl.compileCompositeLit(node)
	case *ast.IndexExpr:
		cl.compileIndexExpr(node)

	default:
		panic(errUnexpectedExpr(cl, node))
	}
}

func (cl *Compiler) compileStmt(node ast.Stmt) {
	switch node := node.(type) {
	case *ast.ReturnStmt:
		cl.compileReturnStmt(node)
	case *ast.BlockStmt:
		cl.compileBlockStmt(node)
	case *ast.IfStmt:
		cl.compileIfStmt(node)
	case *ast.ExprStmt:
		cl.compileExprStmt(node)
	case *ast.DeclStmt:
		cl.compileDeclStmt(node)
	case *ast.AssignStmt:
		cl.compileAssignStmt(node)

	default:
		panic(errUnexpectedStmt(cl, node))
	}
}
