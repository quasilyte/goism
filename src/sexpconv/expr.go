package sexpconv

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"sexp"
)

func Expr(info *types.Info, node ast.Expr) sexp.Form {
	switch node := node.(type) {
	case *ast.Ident:
		return Ident(info, node)
	case *ast.BasicLit:
		return BasicLit(info, node)
	case *ast.BinaryExpr:
		return BinaryExpr(info, node)
	case *ast.ParenExpr:
		return Expr(info, node.X)

	default:
		panic(fmt.Sprintf("unexpected expr: %#v\n", node))
	}
}

func Ident(info *types.Info, node *ast.Ident) sexp.Form {
	// Identifier can be a constant.
	// If it is so, inline constant value.
	if tv := info.Types[node]; tv.Value != nil {
		return Constant(tv.Value)
	}
	return sexp.Var{Name: node.Name}
}

func BasicLit(info *types.Info, node *ast.BasicLit) sexp.Form {
	switch node.Kind {
	case token.INT:
		return constantInt(info.Types[node].Value)
	case token.FLOAT:
		return constantFloat(info.Types[node].Value)
	case token.STRING:
		return constantString(info.Types[node].Value)
	case token.CHAR:
		return constantChar(info.Types[node].Value)

	default:
		panic(fmt.Sprintf("unexpected literal: %#v", node))
	}
}

func BinaryExpr(info *types.Info, node *ast.BinaryExpr) sexp.Form {
	cv := info.Types[node].Value
	if cv != nil {
		return Constant(cv) // Expr result.
	}

	// #FIXME: size information is unused.
	kind := mapKind(info.Types[node.X].Type.(*types.Basic))
	args := []sexp.Form{
		Expr(info, node.X),
		Expr(info, node.Y),
	}

	switch kind.tag + int64(node.Op)<<32 {
	case int64(token.ADD)<<32 + kindInt:
		return &sexp.IntAdd{Args: args}
	case int64(token.ADD)<<32 + kindFloat:
		return &sexp.FloatAdd{Args: args}
	case int64(token.ADD)<<32 + kindString:
		return &sexp.Concat{Args: args}

	case int64(token.SUB)<<32 + kindInt:
		return &sexp.IntSub{Args: args}
	case int64(token.SUB)<<32 + kindFloat:
		return &sexp.FloatSub{Args: args}

	case int64(token.MUL)<<32 + kindInt:
		return &sexp.IntMul{Args: args}
	case int64(token.MUL)<<32 + kindFloat:
		return &sexp.FloatMul{Args: args}

	case int64(token.QUO)<<32 + kindInt:
		return &sexp.IntDiv{Args: args}
	case int64(token.QUO)<<32 + kindFloat:
		return &sexp.FloatDiv{Args: args}

	case int64(token.EQL)<<32 + kindInt:
		return &sexp.IntEq{Args: args}
	case int64(token.EQL)<<32 + kindFloat:
		return &sexp.FloatEq{Args: args}
	case int64(token.EQL)<<32 + kindString:
		return &sexp.StringEq{Args: args}

	case int64(token.LSS)<<32 + kindInt:
		return &sexp.IntLess{Args: args}
	case int64(token.LSS)<<32 + kindFloat:
		return &sexp.FloatLess{Args: args}
	case int64(token.LSS)<<32 + kindString:
		return &sexp.StringLess{Args: args}

	case int64(token.GTR)<<32 + kindInt:
		return &sexp.IntGreater{Args: args}
	case int64(token.GTR)<<32 + kindFloat:
		return &sexp.FloatGreater{Args: args}
	case int64(token.GTR)<<32 + kindString:
		return &sexp.StringGreater{Args: args}

	default:
		panic("unimplemented")
	}
}
