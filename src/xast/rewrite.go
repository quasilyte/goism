package xast

import (
	"fmt"
	"go/ast"
)

type RewriteFunc func(ast.Node) ast.Node

func RewriteExpr(node ast.Expr, f RewriteFunc) ast.Expr {
	return Rewrite(node, f).(ast.Expr)
}

func RewriteStmt(node ast.Stmt, f RewriteFunc) ast.Stmt {
	return Rewrite(node, f).(ast.Stmt)
}

func Rewrite(node ast.Node, f RewriteFunc) ast.Node {
	switch node := node.(type) {
	/* Expressions */

	case *ast.Ident:
		if res := f(node); res != nil {
			return res
		}

	case *ast.BasicLit:
		if res := f(node); res != nil {
			return res
		}

	case *ast.CompositeLit:
		if res := f(node); res != nil {
			return res
		}
		node.Elts = rewriteExprList(node.Elts, f)

	case *ast.CallExpr:
		if res := f(node); res != nil {
			return res
		}
		node.Args = rewriteExprList(node.Args, f)
		node.Fun = RewriteExpr(node.Fun, f)

	case *ast.UnaryExpr:
		if res := f(node); res != nil {
			return res
		}
		node.X = RewriteExpr(node.X, f)

	case *ast.BinaryExpr:
		if res := f(node); res != nil {
			return res
		}
		node.X = RewriteExpr(node.X, f)
		node.Y = RewriteExpr(node.Y, f)

	case *ast.SelectorExpr:
		if res := f(node); res != nil {
			return res
		}
		node.X = RewriteExpr(node.X, f)

	case *ast.IndexExpr:
		if res := f(node); res != nil {
			return res
		}
		node.X = RewriteExpr(node.X, f)
		node.Index = RewriteExpr(node.Index, f)

	/* Statements */

	case *ast.ExprStmt:
		if res := f(node); res != nil {
			return res
		}
		node.X = RewriteExpr(node.X, f)

	case *ast.AssignStmt:
		if res := f(node); res != nil {
			return res
		}
		node.Lhs = rewriteExprList(node.Lhs, f)
		node.Rhs = rewriteExprList(node.Rhs, f)

	case *ast.ReturnStmt:
		if res := f(node); res != nil {
			return res
		}
		node.Results = rewriteExprList(node.Results, f)

	case *ast.IfStmt:
		if res := f(node); res != nil {
			return res
		}
		node.Cond = RewriteExpr(node.Cond, f)
		node.Body = RewriteStmt(node.Body, f).(*ast.BlockStmt)
		node.Else = RewriteStmt(node.Else, f)

	case *ast.BlockStmt:
		if res := f(node); res != nil {
			return res
		}
		node.List = rewriteStmtList(node.List, f)

	/* Declarations */

	case *ast.DeclStmt:
		if res := f(node); res != nil {
			return res
		}
		node.Decl = Rewrite(node.Decl, f).(ast.Decl)

	case *ast.GenDecl:
		if res := f(node); res != nil {
			return res
		}
		for i, spec := range node.Specs {
			node.Specs[i] = Rewrite(spec, f).(ast.Spec)
		}

	case *ast.ValueSpec:
		if res := f(node); res != nil {
			return res
		}

	default:
		panic(fmt.Sprintf("unexpected node: %#v", node))
	}

	return node
}

func rewriteExprList(nodes []ast.Expr, f RewriteFunc) []ast.Expr {
	for i, node := range nodes {
		nodes[i] = RewriteExpr(node, f)
	}
	return nodes
}

func rewriteStmtList(nodes []ast.Stmt, f RewriteFunc) []ast.Stmt {
	for i, node := range nodes {
		nodes[i] = RewriteStmt(node, f)
	}
	return nodes
}
