package compiler

import (
	"go/ast"
	"xast"
)

func preprocessTree(tree *xast.Tree) {
	pp := preprocessor{tree: tree}
	xast.Rewrite(tree.Root, pp.preprocess)
}

type preprocessor struct {
	tree *xast.Tree
}

// Replace operations that are not supported or hard to
// compile into available equivalents.
func (pp preprocessor) preprocess(node ast.Node) ast.Node {
	tree := pp.tree
	e := func(node ast.Expr) ast.Expr {
		return xast.RewriteExpr(node, pp.preprocess)
	}

	switch node := node.(type) {
	case *ast.BinaryExpr:
		x, y := e(node.X), e(node.Y)
		switch tree.OpKinds[node] {
		case xast.OpShr:
			return tree.Shl(x, tree.Neg(y))
		case xast.OpNumEq:
			return tree.Not(tree.NumEq(x, y))
		case xast.OpStrEq:
			return tree.Not(tree.StrEq(x, y))
		}
	}

	return nil
}
