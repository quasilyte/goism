package sexpconv

import (
	"fmt"
	"go/ast"
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
	case *ast.SliceExpr:
		return conv.SliceExpr(node)

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
	return conv.Constant(node)
}

func (conv *Converter) BinaryExpr(node *ast.BinaryExpr) sexp.Form {
	if cv := conv.Constant(node); cv != nil {
		return cv
	}

	typ := conv.basicTypeOf(node.X)
	x, y := conv.Expr(node.X), conv.Expr(node.Y)

	if typ.Info()&types.IsNumeric != 0 {
		switch node.Op {
		case token.ADD:
			return sexp.NewAdd(x, y)
		case token.SUB:
			return sexp.NewSub(x, y)
		case token.MUL:
			return sexp.NewMul(x, y)
		case token.QUO:
			return sexp.NewQuo(x, y)
		case token.EQL:
			return sexp.NewNumEq(x, y)
		case token.LSS:
			return sexp.NewNumLt(x, y)
		case token.GTR:
			return sexp.NewNumGt(x, y)
		case token.LEQ:
			return sexp.NewNumLte(x, y)
		case token.GEQ:
			return sexp.NewNumGte(x, y)
		case token.AND:
			return sexp.NewBitAnd(x, y)
		case token.OR:
			return sexp.NewBitOr(x, y)
		case token.SHL:
			return sexp.NewShl(x, y)
		case token.SHR:
			return sexp.NewShr(x, y)

		default:
			panic(fmt.Sprintf("unexpected num op: %#v", node.Op))
		}
	}

	if typ.Kind() == types.String {
		switch node.Op {
		case token.ADD:
			return sexp.NewConcat(x, y)
		case token.EQL:
			return sexp.NewStrEq(x, y)
		case token.LSS:
			return sexp.NewStrLt(x, y)
		case token.GTR:
			return sexp.NewStrGt(x, y)
		case token.LEQ:
			return sexp.NewStrLte(x, y)
		case token.GEQ:
			return sexp.NewStrGte(x, y)

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

	x := conv.Expr(node.X)

	switch node.Op {
	case token.NOT:
		return sexp.NewNot(x)
	case token.SUB:
		return sexp.NewNeg(x)
	case token.ADD:
		return x
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

	case *types.Slice:
		slice := conv.Expr(node.X)
		index := conv.Expr(node.Index)

		if sexp.Cost(slice) > 4 {
			return _let(slice, &sexp.SliceIndex{
				Slice: _it(typ),
				Index: index,
			})
		}
		return &sexp.SliceIndex{Slice: slice, Index: index}

	// #TODO: strings
	default:
		panic("unimplemented")
	}
}

func (conv *Converter) SliceExpr(node *ast.SliceExpr) sexp.Form {
	var low, high sexp.Form
	if node.Low != nil {
		low = conv.Expr(node.Low)
	}
	if node.High != nil {
		high = conv.Expr(node.High)
	}
	if isArray(conv.typeOf(node.X)) {
		return sexp.NewArraySlice(conv.Expr(node.X), low, high)
	}
	return sexp.NewSubslice(conv.Expr(node.X), low, high)
}

func (conv *Converter) CompositeLit(node *ast.CompositeLit) sexp.Form {
	switch typ := conv.typeOf(node).(type) {
	case *types.Array:
		return conv.arrayLit(node, typ)
	case *types.Slice:
		return conv.sliceLit(node, typ)

	default:
		panic(fmt.Sprintf("unexpected comp. lit: %#v\n", node))
	}
}

func (conv *Converter) sliceLit(node *ast.CompositeLit, typ *types.Slice) sexp.Form {
	if len(node.Elts) == 0 {
		return ZeroValue(typ)
	}

	return &sexp.SliceLit{Vals: conv.exprList(node.Elts), Typ: typ}
}

func (conv *Converter) arrayLit(node *ast.CompositeLit, typ *types.Array) sexp.Form {
	if len(node.Elts) == 0 {
		return ZeroValue(typ)
	}

	if len(node.Elts) != int(typ.Len()) {
		vals := make(map[int]sexp.Form, len(node.Elts))
		for i, elt := range node.Elts {
			vals[i] = conv.Expr(elt)
		}
		ctor := &sexp.Call{
			Fn: function.MakeVector,
			Args: []sexp.Form{
				sexp.Int(typ.Len()),
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
