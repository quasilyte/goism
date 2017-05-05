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
	case *ast.ParenExpr:
		return Expr(info, node.X)
	case *ast.Ident:
		return Ident(info, node)
	case *ast.BasicLit:
		return BasicLit(info, node)
	case *ast.BinaryExpr:
		return BinaryExpr(info, node)
	case *ast.CallExpr:
		return CallExpr(info, node)
	case *ast.SelectorExpr:
		return SelectorExpr(info, node)
	case *ast.TypeAssertExpr:
		return TypeAssertExpr(info, node)
	case *ast.IndexExpr:
		return IndexExpr(info, node)

	default:
		panic(fmt.Sprintf("unexpected expr: %#v\n", node))
	}
}

func Ident(info *types.Info, node *ast.Ident) sexp.Form {
	if cv := info.Types[node].Value; cv != nil {
		return Constant(cv)
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
	if cv := info.Types[node].Value; cv != nil {
		return Constant(cv)
	}

	typ := info.TypeOf(node.X).(*types.Basic)
	args := []sexp.Form{
		Expr(info, node.X),
		Expr(info, node.Y),
	}

	if typ.Info()&types.IsNumeric != 0 {
		switch node.Op {
		case token.ADD:
			return &sexp.NumAdd{Type: typ, Args: args}
		case token.SUB:
			return &sexp.NumSub{Type: typ, Args: args}
		case token.MUL:
			return &sexp.NumMul{Type: typ, Args: args}
		case token.QUO:
			return &sexp.NumQuo{Type: typ, Args: args}
		case token.EQL:
			return &sexp.NumEq{Type: typ, Args: args}
		case token.LSS:
			return &sexp.NumLt{Type: typ, Args: args}
		case token.GTR:
			return &sexp.NumGt{Type: typ, Args: args}
		case token.LEQ:
			return &sexp.NumLte{Type: typ, Args: args}
		case token.GEQ:
			return &sexp.NumGte{Type: typ, Args: args}

		default:
			panic(fmt.Sprintf("unexpected num op: %#v", node.Op))
		}
	}

	if typ.Kind() == types.String {
		switch node.Op {
		case token.ADD:
			return &sexp.Concat{Args: args}
		case token.EQL:
			return &sexp.StringEq{Args: args}
		case token.LSS:
			return &sexp.StringLt{Args: args}
		case token.GTR:
			return &sexp.StringGt{Args: args}
		case token.LEQ:
			return &sexp.StringLte{Args: args}
		case token.GEQ:
			return &sexp.StringGte{Args: args}

		default:
			panic(fmt.Sprintf("unexpected string op: %#v", node.Op))
		}
	}

	panic("unimplemented")
}

func CallExpr(info *types.Info, node *ast.CallExpr) sexp.Form {
	// #REFS: 2.
	switch fn := node.Fun.(type) {
	case *ast.SelectorExpr:
		if obj, ok := fn.X.(*ast.Ident); ok {
			if obj.Name == "emacs" {
				return intrinsic(info, fn.Sel.Name, node.Args)
			}

			qualName := obj.Name + "." + fn.Sel.Name
			return call(info, qualName, node.Args...)
		}
		panic(fmt.Sprintf("unexpected selector: %#v", fn))

	case *ast.Ident:
		if fn.Name == "make" {
			return makeBuiltin(info, node.Args)
		}
		return call(info, fn.Name, node.Args...)

	default:
		panic(fmt.Sprintf("unexpected func: %#v", node.Fun))
	}
}

func SelectorExpr(info *types.Info, node *ast.SelectorExpr) sexp.Form {
	if cv := info.Types[node].Value; cv != nil {
		return Constant(cv)
	}

	sel := info.Selections[node]
	if sel != nil {
		panic("unimplemented")
	}

	panic(fmt.Sprintf("unexpected selector: %#v", node))
}

func TypeAssertExpr(info *types.Info, node *ast.TypeAssertExpr) sexp.Form {
	panic("unimplemented")
}

func IndexExpr(info *types.Info, node *ast.IndexExpr) sexp.Form {
	switch typ := info.Types[node.X].Type; typ.(type) {
	case *types.Map:
		return call(info, "gethash", node.Index, node.X)

	// #TODO: arrays, slices, strings
	default:
		panic("unimplemented")
	}
}
