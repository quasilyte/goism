package xast

import (
	"go/ast"
	"go/token"
	"go/types"
)

func simplifyTree(tree *Tree) {
	sim := newSimplifier(tree)
	RewriteStmt(tree.Root, sim.simplify)
}

type simplifier struct {
	tree  *Tree
	block []ast.Stmt // Block being processed
}

func newSimplifier(tree *Tree) *simplifier {
	return &simplifier{
		tree: tree,
	}
}

func (sim *simplifier) simplify(node ast.Node) ast.Node {
	tree := sim.tree

	switch node := node.(type) {
	case *ast.BasicLit:
		tree.internLit(node)

	case *ast.BinaryExpr:
		typ := tree.TypeOf(node).(*types.Basic)
		tree.OpKinds[node] = OpKind(typ, node.Op)

	case *ast.BlockStmt:
		node.List = sim.simplifyBlockBody(node.List)
		return node

	case *ast.DeclStmt:
		decl := node.Decl.(*ast.GenDecl)
		if decl.Tok == token.CONST {
			// #TODO: ensure that constants are available.
			return EmptyStmt
		}
		if decl.Tok == token.TYPE {
			return EmptyStmt
		}
		return sim.simplifyVarSpecs(decl.Specs)

	case *ast.AssignStmt:
		if node.Tok == token.ASSIGN {
			return sim.simplifyAssign(node)
		}
		return sim.simplifyDefine(node)
	}

	return nil
}

func (sim *simplifier) simplifyBlockBody(body []ast.Stmt) []ast.Stmt {
	block := sim.block
	defer func() { sim.block = block }()

	sim.block = make([]ast.Stmt, 0, len(body))
	for _, stmt := range body {
		// If empty statement is returned, most likely
		// new statements were appended by side effects.
		if node := RewriteStmt(stmt, sim.simplify); node != EmptyStmt {
			sim.block = append(sim.block, node)
		}
	}

	return sim.block
}

/*
	x, y = f()
	a, b = 1, 2
=>
	x = f().0
	y = f().1
	a = 1
	b = 2
*/
func (sim *simplifier) simplifyAssign(node *ast.AssignStmt) *ast.EmptyStmt {
	tree := sim.tree
	values := sim.valueList(len(node.Lhs), node.Rhs)
	for i, lhs := range node.Lhs {
		sim.block = append(sim.block, tree.NewRebind(lhs, values[i]))
	}
	return EmptyStmt
}

/*
	var y = 0
	x, y := f()
=>
	var y = 0
	var x = f().0
	y = f().1
*/
func (sim *simplifier) simplifyDefine(node *ast.AssignStmt) *ast.EmptyStmt {
	tree := sim.tree
	values := sim.valueList(len(node.Lhs), node.Rhs)
	for i, lhs := range node.Lhs {
		if ident, ok := lhs.(*ast.Ident); ok && tree.Defs[ident] != nil {
			sim.block = append(sim.block, tree.NewBind(ident, values[i]))
		} else {
			sim.block = append(sim.block, tree.NewRebind(lhs, values[i]))
		}
	}
	return EmptyStmt
}

/*
	var (
		x, y int
		a, b = f()
	)
=>
	var x = 0
	var y = 0
	var a = f().0
	var b = f().1
*/
func (sim *simplifier) simplifyVarSpecs(specs []ast.Spec) *ast.EmptyStmt {
	tree := sim.tree
	for _, spec := range specs {
		spec := spec.(*ast.ValueSpec)
		var values []ast.Expr

		if len(spec.Values) == 0 {
			// Need to intern ZV.
			zv := ZeroValue(tree.TypeOf(spec.Type))
			values = sim.valueToList(len(spec.Names), zv)
		} else {
			values = sim.valueList(len(spec.Names), spec.Values)
		}

		for i, ident := range spec.Names {
			val := RewriteExpr(values[i], sim.simplify)
			sim.block = append(sim.block, tree.NewBind(ident, val))
		}
	}
	return EmptyStmt
}

// Creates a slice with length=count and each value initialized to
// specified initializer.
func (sim *simplifier) valueToList(count int, init ast.Expr) []ast.Expr {
	res := make([]ast.Expr, count)
	for i := range res {
		res[i] = init
	}
	return res
}

// For single value assignment returns passed values argument.
// For multi value assignment returns expanded values list.
func (sim *simplifier) valueList(count int, values []ast.Expr) []ast.Expr {
	if len(values) == count {
		return rewriteExprList(values, sim.simplify)
	}
	res := make([]ast.Expr, count)
	res[0] = RewriteExpr(values[0], sim.simplify)
	for i := 1; i < count; i++ {
		res[i] = multiRetIdent[i]
	}
	return res
}
