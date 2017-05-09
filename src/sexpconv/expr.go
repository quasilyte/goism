package sexpconv

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"lisp"
	"sexp"
)

func (conv *Converter) Expr(node ast.Expr) sexp.Form {
	switch node := node.(type) {
	case *ast.ParenExpr:
		return conv.Expr(node.X)
	case *ast.Ident:
		return conv.Ident(node)
	case *ast.BasicLit:
		return conv.BasicLit(node)
	case *ast.BinaryExpr:
		return conv.BinaryExpr(node)
	case *ast.CallExpr:
		return conv.CallExpr(node)
	case *ast.SelectorExpr:
		return conv.SelectorExpr(node)
	case *ast.TypeAssertExpr:
		return conv.TypeAssertExpr(node)
	case *ast.IndexExpr:
		return conv.IndexExpr(node)

	default:
		panic(fmt.Sprintf("unexpected expr: %#v\n", node))
	}
}

func (conv *Converter) Ident(node *ast.Ident) sexp.Form {
	if cv := conv.Constant(node); cv != nil {
		return cv
	}
	return sexp.Var{Name: node.Name}
}

func (conv *Converter) BasicLit(node *ast.BasicLit) sexp.Form {
	typ := conv.typeOf(node)

	if types.Identical(typ, lisp.Types.Symbol) {
		return sexp.Symbol{Val: constant.StringVal(conv.valueOf(node))}
	}

	info := conv.typeOf(node).Underlying().(*types.Basic).Info()

	if info&types.IsFloat != 0 {
		return constantFloat(conv.valueOf(node))
	}
	if info&types.IsString != 0 {
		return constantString(conv.valueOf(node))
	}
	if node.Kind == token.CHAR {
		return constantChar(conv.valueOf(node))
	}
	return constantInt(conv.valueOf(node))
}

func (conv *Converter) BinaryExpr(node *ast.BinaryExpr) sexp.Form {
	if cv := conv.Constant(node); cv != nil {
		return cv
	}

	typ := conv.basicTypeOf(node.X)
	args := []sexp.Form{
		conv.Expr(node.X),
		conv.Expr(node.Y),
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

func (conv *Converter) CallExpr(node *ast.CallExpr) sexp.Form {
	// #REFS: 2.
	switch fn := node.Fun.(type) {
	case *ast.SelectorExpr: // x.sel()
		obj := fn.X.(*ast.Ident)
		if obj.Name == "lisp" {
			return conv.intrinsic(fn.Sel.Name, node.Args)
		}

		qualName := "Go-" + obj.Name + "." + fn.Sel.Name
		return conv.call(qualName, node.Args...)

	case *ast.Ident: // f()
		switch fn.Name {
		case "make":
			return conv.makeBuiltin(node.Args)
		case "panic":
			return &sexp.Panic{ErrorData: conv.Expr(node.Args[0])}
		case "int", "string", "float64":
			return conv.Expr(node.Args[0])
		case "print":
			return conv.call("Go--print", node.Args...)
		case "println":
			return conv.call("Go--println", node.Args...)
		default:
			return conv.call(conv.symPrefix+fn.Name, node.Args...)
		}
	default:
		panic(fmt.Sprintf("unexpected func: %#v", node.Fun))
	}
}

func (conv *Converter) SelectorExpr(node *ast.SelectorExpr) sexp.Form {
	if cv := conv.Constant(node); cv != nil {
		return cv
	}

	sel := conv.info.Selections[node]
	if sel != nil {
		panic("unimplemented")
	}

	panic(fmt.Sprintf("unexpected selector: %#v", node))
}

func (conv *Converter) TypeAssertExpr(node *ast.TypeAssertExpr) sexp.Form {
	exprTyp := conv.typeOf(node.X)
	expr := conv.Expr(node.X)
	assertTyp := conv.typeOf(node.Type)

	if exprTyp.Underlying() == lisp.Types.Object {
		return &sexp.LispTypeAssert{Expr: expr, Type: assertTyp}
	}
	return &sexp.TypeAssert{Expr: expr, Type: assertTyp}
}

func (conv *Converter) IndexExpr(node *ast.IndexExpr) sexp.Form {
	switch typ := conv.typeOf(node.X); typ.(type) {
	case *types.Map:
		return conv.call("gethash", node.Index, node.X)

	// #TODO: arrays, slices, strings
	default:
		panic("unimplemented")
	}
}

func (conv *Converter) Constant(node ast.Expr) sexp.Form {
	if cv := conv.valueOf(node); cv != nil {
		typ := conv.typeOf(node)
		if types.Identical(typ, lisp.Types.Symbol) {
			return sexp.Symbol{Val: constant.StringVal(cv)}
		}
		return Constant(cv)
	}
	return nil
}
