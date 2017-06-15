package sexpconv

import (
	"exn"
	"go/ast"
	"go/token"
	"go/types"
	"magic_pkg/emacs/rt"
	"sexp"
	"sys_info/function"
	"xtypes"
)

func (conv *converter) Expr(node ast.Expr) sexp.Form {
	switch node := node.(type) {
	case *ast.ParenExpr:
		return conv.Expr(node.X)
	case *ast.Ident:
		return conv.Ident(node)
	case *ast.BasicLit:
		return conv.Constant(node)
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
		panic(errUnexpectedExpr(conv, node))
	}
}

func (conv *converter) Ident(node *ast.Ident) sexp.Form {
	if cv := conv.Constant(node); cv != nil {
		return cv
	}

	obj := conv.info.Uses[node]
	typ := obj.Type()

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

	if xtypes.IsGlobal(obj) {
		return sexp.Var{Name: conv.env.InternVar(nil, node.Name), Typ: typ}
	}
	return sexp.Var{Name: node.Name, Typ: typ}
}

func (conv *converter) BinaryExpr(node *ast.BinaryExpr) sexp.Form {
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
		case token.NEQ:
			return sexp.NewNumNeq(x, y)
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
			panic(errUnexpectedExpr(conv, node))
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
			panic(errUnexpectedExpr(conv, node))
			// return sexp.NewStrLte(x, y)
		case token.GEQ:
			panic(errUnexpectedExpr(conv, node))
			// return sexp.NewStrGte(x, y)

		default:
			panic(errUnexpectedExpr(conv, node))
		}
	}

	if node.Op == token.LAND {
		return &sexp.And{X: x, Y: y}
	}
	if node.Op == token.LOR {
		return &sexp.Or{X: x, Y: y}
	}

	panic(errUnexpectedExpr(conv, node))
}

func (conv *converter) structIndex(typ *types.Struct, node *ast.SelectorExpr) *sexp.StructIndex {
	return &sexp.StructIndex{
		Struct: conv.Expr(node.X),
		Index:  xtypes.LookupField(node.Sel.Name, typ),
		Typ:    typ,
	}
}

func (conv *converter) SelectorExpr(node *ast.SelectorExpr) sexp.Form {
	if cv := conv.Constant(node); cv != nil {
		return cv
	}

	typ := conv.typeOf(node.X)
	if typ, ok := typ.Underlying().(*types.Struct); ok {
		return conv.structIndex(typ, node)
	}
	if typ, ok := typ.(*types.Pointer); ok {
		// Single level of indirection for structs is the same
		// as not having indirection at all.
		if derefTyp, ok := typ.Elem().Underlying().(*types.Struct); ok {
			return conv.structIndex(derefTyp, node)
		}
	}

	id, ok := node.X.(*ast.Ident)
	if !ok {
		panic(errUnexpectedExpr(conv, node))
	}
	switch obj := conv.info.Uses[id].(type) {
	case *types.PkgName:
		return sexp.Symbol{
			Val: conv.env.InternVar(obj.Imported(), node.Sel.Name),
		}
	default:
		panic(errUnexpectedExpr(conv, node))
	}
}

func (conv *converter) UnaryExpr(node *ast.UnaryExpr) sexp.Form {
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
	case token.AND:
		return conv.takeAddr(x)
	}

	panic(errUnexpectedExpr(conv, node))
}

func (conv *converter) takeAddr(form sexp.Form) sexp.Form {
	if xtypes.IsStruct(form.Type()) {
		return form // #REFS: 25
	}

	panic(exn.NoImpl("take address operation"))
}

func (conv *converter) TypeAssertExpr(node *ast.TypeAssertExpr) sexp.Form {
	expr := conv.Expr(node.X)
	assertTyp := conv.typeOf(node.Type)
	return &sexp.TypeAssert{Expr: expr, Typ: assertTyp}
}

func (conv *converter) IndexExpr(node *ast.IndexExpr) sexp.Form {
	switch typ := conv.typeOf(node.X).(type) {
	case *types.Map:
		return &sexp.LispCall{
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
		return conv.call(rt.FnSliceGet, node.X, node.Index)

	// #TODO: strings
	default:
		panic(errUnexpectedExpr(conv, node))
	}
}

func (conv *converter) SliceExpr(node *ast.SliceExpr) sexp.Form {
	var low, high sexp.Form
	if node.Low != nil {
		low = conv.Expr(node.Low)
	}
	if node.High != nil {
		high = conv.Expr(node.High)
	}
	x := conv.Expr(node.X)
	switch conv.typeOf(node.X).Underlying().(type) {
	case *types.Array:
		return sexp.NewArraySlice(x, low, high)
	case *types.Basic:
		return sexp.NewSubstr(x, low, high)
	default:
		return sexp.NewSubslice(x, low, high)
	}
}

func (conv *converter) CompositeLit(node *ast.CompositeLit) sexp.Form {
	switch typ := conv.typeOf(node).(type) {
	case *types.Array:
		return conv.arrayLit(node, typ)
	case *types.Slice:
		return conv.sliceLit(node, typ)
	case *types.Named:
		return conv.structLit(node, typ.Underlying().(*types.Struct))

	default:
		panic(errUnexpectedExpr(conv, node))
	}
}

func (conv *converter) sliceLit(node *ast.CompositeLit, typ *types.Slice) sexp.Form {
	if len(node.Elts) == 0 {
		return ZeroValue(typ)
	}

	return &sexp.SliceLit{Vals: conv.exprList(node.Elts), Typ: typ}
}

func (conv *converter) arrayLit(node *ast.CompositeLit, typ *types.Array) sexp.Form {
	if len(node.Elts) == 0 {
		return ZeroValue(typ)
	}

	if len(node.Elts) != int(typ.Len()) {
		vals := make(map[int]sexp.Form, len(node.Elts))
		for i, elt := range node.Elts {
			vals[i] = conv.Expr(elt)
		}
		ctor := &sexp.LispCall{
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

func (conv *converter) structLit(node *ast.CompositeLit, typ *types.Struct) sexp.Form {
	vals := make([]sexp.Form, typ.NumFields())
	for _, elt := range node.Elts {
		kv := elt.(*ast.KeyValueExpr)
		key := kv.Key.(*ast.Ident)
		vals[xtypes.LookupField(key.Name, typ)] = conv.valueCopy(conv.Expr(kv.Value))
	}
	for i, val := range vals {
		if val == nil {
			vals[i] = ZeroValue(typ.Field(i).Type())
		}
	}
	return &sexp.StructLit{Vals: vals, Typ: typ}
}
