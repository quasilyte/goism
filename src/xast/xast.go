package xast

import (
	"go/ast"
	"go/token"
	"go/types"
	"lisp"
)

func NewTree(ti *types.Info, root *ast.BlockStmt) *Tree {
	tree := &Tree{
		Info:    ti,
		Root:    root,
		OpKinds: make(map[*ast.BinaryExpr]int),
		Literals: map[*ast.BasicLit]interface{}{
			zeroInt:   int64(0),
			zeroFloat: float64(0.0),
			zeroStr:   "",
		},
	}

	simplifyTree(tree)

	return tree
}

const (
	OpNumAdd = iota
	OpNumSub
	OpNumMul
	OpNumQuo
	OpNumEq
	OpNumNotEq
	OpNumLt
	OpNumGt
	OpNumLte
	OpNumGte
	OpBitAnd
	OpBitOr
	OpBitXor
	OpShl
	OpShr

	OpConcat
	OpStrEq
	OpStrNotEq
	OpStrLt
	OpStrGt
	OpStrLte
	OpStrGte

	OpUnsupported // Should be the last
)

func OpKind(typ *types.Basic, op token.Token) int {
	info := typ.Info()
	if info&types.IsInteger != 0 || info&types.IsFloat != 0 {
		switch op {
		case token.ADD:
			return OpNumAdd
		case token.SUB:
			return OpNumSub
		case token.MUL:
			return OpNumMul
		case token.QUO:
			return OpNumQuo
		case token.EQL:
			return OpNumEq
		case token.NEQ:
			return OpNumNotEq
		case token.LSS:
			return OpNumLt
		case token.GTR:
			return OpNumGt
		case token.LEQ:
			return OpNumLte
		case token.GEQ:
			return OpNumGte
		case token.AND:
			return OpBitAnd
		case token.OR:
			return OpBitOr
		case token.XOR:
			return OpBitXor
		case token.SHL:
			return OpShl
		case token.SHR:
			return OpShr
		default:
			return OpUnsupported
		}
	} else if typ.Kind() == types.String {
		switch op {
		case token.EQL:
			return OpStrEq
		case token.NEQ:
			return OpStrNotEq
		case token.LSS:
			return OpStrLt
		case token.GTR:
			return OpStrGt
		case token.LEQ:
			return OpStrLte
		case token.GEQ:
			return OpStrGte
		default:
			return OpUnsupported
		}
	} else {
		return OpUnsupported
	}
}

func IsNoreturnExpr(node ast.Expr) bool {
	if node, ok := node.(*ast.CallExpr); ok {
		return IsNoreturnCall(node)
	}
	return false
}

func IsNoreturnCall(node *ast.CallExpr) bool {
	if ident, ok := node.Fun.(*ast.Ident); ok {
		if lisp.IsNoreturn(ident.Name) {
			return true
		}
	}
	return false
}
