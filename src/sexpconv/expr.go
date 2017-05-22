package sexpconv

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"lisp"
	"lisp/function"
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
	case *ast.UnaryExpr:
		return conv.UnaryExpr(node)
	case *ast.CompositeLit:
		return conv.CompositeLit(node)

	default:
		panic(fmt.Sprintf("unexpected expr: %#v\n", node))
	}
}

func (conv *Converter) Ident(node *ast.Ident) sexp.Form {
	if cv := conv.Constant(node); cv != nil {
		return cv
	}

	typ := conv.typeOf(node)

	if typ, ok := typ.(*types.Basic); ok {
		// Coerce untyped nil to correct value depending on
		// the context type.
		if typ.Kind() == types.UntypedNil {
			switch conv.ctxType.(type) {
			case *types.Map:
				return nilMap
			case *types.Slice:
				return nilSlice
			case *types.Signature:
				return nilFunc
			case *types.Interface:
				return nilInterface
			}
		}
	}

	return sexp.Var{
		Name: node.Name,
		Typ:  typ,
	}
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
	return constantInt(conv.valueOf(node))
}

func (conv *Converter) BinaryExpr(node *ast.BinaryExpr) sexp.Form {
	if cv := conv.Constant(node); cv != nil {
		return cv
	}

	typ := conv.basicTypeOf(node.X)
	args := [2]sexp.Form{
		conv.Expr(node.X),
		conv.Expr(node.Y),
	}

	if typ.Info()&types.IsNumeric != 0 {
		switch node.Op {
		case token.ADD:
			return &sexp.NumAdd{Typ: typ, Args: args}
		case token.SUB:
			return &sexp.NumSub{Typ: typ, Args: args}
		case token.MUL:
			return &sexp.NumMul{Typ: typ, Args: args}
		case token.QUO:
			return &sexp.NumQuo{Typ: typ, Args: args}
		case token.EQL:
			return &sexp.NumEq{Typ: typ, Args: args}
		case token.LSS:
			return &sexp.NumLt{Typ: typ, Args: args}
		case token.GTR:
			return &sexp.NumGt{Typ: typ, Args: args}
		case token.LEQ:
			return &sexp.NumLte{Typ: typ, Args: args}
		case token.GEQ:
			return &sexp.NumGte{Typ: typ, Args: args}

		default:
			panic(fmt.Sprintf("unexpected num op: %#v", node.Op))
		}
	}

	if typ.Kind() == types.String {
		switch node.Op {
		case token.ADD:
			return &sexp.Concat{Args: args[:]}
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

func (conv *Converter) UnaryExpr(node *ast.UnaryExpr) sexp.Form {
	if cv := conv.Constant(node); cv != nil {
		return cv
	}

	panic("unimplemented")
}

func (conv *Converter) TypeAssertExpr(node *ast.TypeAssertExpr) sexp.Form {
	exprTyp := conv.typeOf(node.X)
	expr := conv.Expr(node.X)
	assertTyp := conv.typeOf(node.Type)

	if exprTyp.Underlying() == lisp.Types.Object {
		return &sexp.LispTypeAssert{Expr: expr, Typ: assertTyp}
	}
	return &sexp.TypeAssert{Expr: expr, Typ: assertTyp}
}

func (conv *Converter) IndexExpr(node *ast.IndexExpr) sexp.Form {
	switch typ := conv.typeOf(node.X).(type) {
	case *types.Map:
		return &sexp.Call{
			Fn: function.Gethash,
			Args: conv.valueCopyList([]sexp.Form{
				conv.Expr(node.Index),
				conv.Expr(node.X),
				ZeroValue(typ.Elem()),
			}),
		}

	case *types.Array:
		return &sexp.ArrayIndex{
			Array: conv.Expr(node.X),
			Index: conv.Expr(node.Index),
		}

	// #TODO: slices, strings
	default:
		panic("unimplemented")
	}
}

func (conv *Converter) CompositeLit(node *ast.CompositeLit) sexp.Form {
	switch typ := conv.typeOf(node).(type) {
	case *types.Array:
		return conv.arrayLit(node, typ)

	default:
		panic(fmt.Sprintf("unexpected comp. lit: %#v\n", node))
	}
}

func (conv *Converter) arrayLit(node *ast.CompositeLit, typ *types.Array) sexp.Form {
	if len(node.Elts) == 0 {
		return ZeroValue(typ)
	}

	if len(node.Elts) != int(typ.Len()) {
		vals := make([]sexp.SparseArrayVal, len(node.Elts))
		for i, elt := range node.Elts {
			vals[i] = sexp.SparseArrayVal{
				Index: int64(i),
				Expr:  conv.Expr(elt),
			}
		}
		ctor := &sexp.Call{
			Fn: function.MakeVector,
			Args: []sexp.Form{
				sexp.Int{Val: typ.Len()},
				ZeroValue(typ.Elem()),
			},
		}
		return &sexp.SparseArrayLit{
			Vals: vals,
			Ctor: ctor,
			Typ:  typ,
		}
	}

	// Each element has explicit initializer.
	return &sexp.ArrayLit{Vals: conv.exprList(node.Elts), Typ: typ}
}
