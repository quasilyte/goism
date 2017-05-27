package xast

import (
	"assert"
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
)

// Tree wraps rewritten ast.Block and stores metadata for it.
// It also serves as a node factory.
type Tree struct {
	*types.Info

	Root *ast.BlockStmt

	// Used to disambiguate operators like "+", which can
	// mean different things depending on their operands type.
	OpKinds map[*ast.BinaryExpr]int

	// Literal values storage.
	// New literal AST nodes should be created by Env object
	// to avoid missing entries.
	Literals map[*ast.BasicLit]interface{}

	retVars []*ast.Ident
}

func (tree *Tree) NewBind(ident *ast.Ident, init ast.Expr) *ast.DeclStmt {
	return &ast.DeclStmt{
		Decl: &ast.GenDecl{
			Tok: token.VAR,
			Specs: []ast.Spec{
				&ast.ValueSpec{
					Names:  []*ast.Ident{ident},
					Values: []ast.Expr{init},
				},
			},
		},
	}
}
func (tree *Tree) NewRebind(lhs, rhs ast.Expr) *ast.AssignStmt {
	return &ast.AssignStmt{
		Tok: token.ASSIGN,
		Lhs: []ast.Expr{lhs},
		Rhs: []ast.Expr{rhs},
	}
}

func (tree *Tree) Neg(x ast.Expr) *ast.UnaryExpr {
	return &ast.UnaryExpr{Op: token.SUB, X: x}
}
func (tree *Tree) Not(x ast.Expr) *ast.UnaryExpr {
	return &ast.UnaryExpr{Op: token.NOT, X: x}
}

func (tree *Tree) NumEq(x, y ast.Expr) *ast.BinaryExpr {
	return tree.binaryExpr(x, y, OpNumEq)
}
func (tree *Tree) StrEq(x, y ast.Expr) *ast.BinaryExpr {
	return tree.binaryExpr(x, y, OpStrEq)
}
func (tree *Tree) Shl(x, y ast.Expr) *ast.BinaryExpr {
	return tree.binaryExpr(x, y, OpShl)
}
func (tree *Tree) binaryExpr(x, y ast.Expr, kind int) *ast.BinaryExpr {
	tok := opKindToToken[kind]
	node := &ast.BinaryExpr{Op: tok, X: x, Y: y}
	tree.OpKinds[node] = kind
	return node
}

func (tree *Tree) NewInt(val int64) *ast.BasicLit {
	return tree.newLit(val, token.INT)
}
func (tree *Tree) NewFloat(val float64) *ast.BasicLit {
	return tree.newLit(val, token.FLOAT)
}
func (tree *Tree) NewStr(val string) *ast.BasicLit {
	return tree.newLit(val, token.STRING)
}
func (tree *Tree) newLit(val interface{}, kind token.Token) *ast.BasicLit {
	const internedLitVal = "?"
	node := &ast.BasicLit{Kind: kind, Value: internedLitVal}
	tree.Literals[node] = val
	return node
}

func (tree *Tree) internLit(node *ast.BasicLit) {
	switch cv := tree.Types[node].Value; cv.Kind() {
	case constant.Int:
		val, exact := constant.Int64Val(cv)
		assert.True(exact)
		tree.Literals[node] = val

	case constant.Float:
		val, exact := constant.Float64Val(cv)
		assert.True(exact)
		tree.Literals[node] = val

	case constant.String:
		tree.Literals[node] = constant.StringVal(cv)
	}
}

var opKindToToken = [...]token.Token{
	OpNumAdd:   token.ADD,
	OpNumSub:   token.SUB,
	OpNumMul:   token.MUL,
	OpNumQuo:   token.QUO,
	OpNumEq:    token.EQL,
	OpNumNotEq: token.NEQ,
	OpNumLt:    token.LSS,
	OpNumGt:    token.GTR,
	OpNumLte:   token.LEQ,
	OpNumGte:   token.GEQ,
	OpBitAnd:   token.AND,
	OpBitOr:    token.OR,
	OpBitXor:   token.XOR,
	OpShl:      token.SHL,
	OpShr:      token.SHR,

	OpConcat:   token.ADD,
	OpStrEq:    token.EQL,
	OpStrNotEq: token.NEQ,
	OpStrLt:    token.LSS,
	OpStrGt:    token.GTR,
	OpStrLte:   token.LEQ,
	OpStrGte:   token.GEQ,

	OpUnsupported: token.ILLEGAL,
}
