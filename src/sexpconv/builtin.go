package sexpconv

import (
	"exn"
	"go/ast"
	"go/types"
	"magic_pkg/emacs/rt"
	"sexp"
	"sys_info/function"
)

func (conv *Converter) lenBuiltin(arg ast.Expr) sexp.Form {
	switch typ := conv.typeOf(arg).(type) {
	case *types.Map:
		return conv.lispCall(function.HashTableCount, arg)

	case *types.Array:
		return sexp.Int(typ.Len())

	case *types.Slice:
		return conv.call(rt.FnSliceLen, arg)

	default:
		panic(exn.Conv(conv.fileSet, "can't apply len", arg))
	}
}

func (conv *Converter) capBuiltin(arg ast.Expr) sexp.Form {
	switch typ := conv.typeOf(arg).(type) {
	case *types.Array:
		return sexp.Int(typ.Len())

	case *types.Slice:
		return conv.call(rt.FnSliceCap, arg)

	default:
		panic(exn.Conv(conv.fileSet, "can't apply cap", arg))
	}
}

func (conv *Converter) makeBuiltin(args []ast.Expr) sexp.Form {
	switch typ := conv.typeOf(args[0]).(type) {
	case *types.Map:
		if len(args) == 2 {
			return conv.lispCall(function.MakeMapCap, args[1])
		}
		return conv.lispCall(function.MakeMap)

	case *types.Slice:
		zv := ZeroValue(typ.Elem())
		if len(args) == 2 {
			return conv.lispCall(function.MakeSliceCap, args[1], zv)
		}
		return conv.lispCall(function.MakeSlice, args[1], args[2], zv)

	default:
		panic(exn.Conv(conv.fileSet, "can't make", args[0]))
	}
}

func (conv *Converter) appendBuiltin(args []ast.Expr) sexp.Form {
	if len(args) != 2 {
		panic(exn.NoImpl("variadic append"))
	}

	return conv.lispCall(function.AppendOne, args[0], args[1])
	// typ := conv.typeOf(args[0]).(*types.Slice)
	// return conv.callExprList(function.AppendOne(typ), args)
}
