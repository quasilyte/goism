package sexpconv

import (
	"exn"
	"go/ast"
	"go/types"
	"lisp/function"
	"sexp"
)

func (conv *Converter) lenBuiltin(arg ast.Expr) sexp.Form {
	switch typ := conv.typeOf(arg).(type) {
	case *types.Map:
		return conv.call(function.HashTableCount, arg)

	case *types.Array:
		return sexp.Int(typ.Len())

	case *types.Slice:
		return sexp.NewSliceLen(conv.Expr(arg))

	default:
		panic(exn.Conv(conv.fileSet, "can't apply len", arg))
	}
}

func (conv *Converter) capBuiltin(arg ast.Expr) sexp.Form {
	switch typ := conv.typeOf(arg).(type) {
	case *types.Array:
		return sexp.Int(typ.Len())

	case *types.Slice:
		return sexp.NewSliceCap(conv.Expr(arg))

	default:
		panic(exn.Conv(conv.fileSet, "can't apply cap", arg))
	}
}

func (conv *Converter) makeBuiltin(args []ast.Expr) sexp.Form {
	switch typ := conv.typeOf(args[0]).(type) {
	case *types.Map:
		if len(args) == 2 {
			return conv.call(function.MakeMapCap(typ), args[1])
		}
		return conv.call(function.MakeMap(typ))

	case *types.Slice:
		zv := ZeroValue(typ.Elem())
		if len(args) == 2 {
			return conv.call(function.MakeSliceCap(typ), args[1], zv)
		}
		return conv.call(function.MakeSlice(typ), args[1], args[2], zv)

	default:
		panic(exn.Conv(conv.fileSet, "can't make", args[0]))
	}
}

func (conv *Converter) appendBuiltin(args []ast.Expr) sexp.Form {
	if len(args) != 2 {
		panic(exn.NoImpl("variadic append"))
	}

	typ := conv.typeOf(args[0]).(*types.Slice)
	return conv.callExprList(function.AppendOne(typ), args)
}
