package sexpconv

import (
	"exn"
	"go/ast"
	"go/types"
	"magic_pkg/emacs/lisp"
	"magic_pkg/emacs/rt"
	"sexp"
)

func (conv *converter) lenBuiltin(arg ast.Expr) sexp.Form {
	switch typ := conv.typeOf(arg).(type) {
	case *types.Map:
		return conv.lispCall(lisp.FnHashTableCount, arg)

	case *types.Array:
		return sexp.Int(typ.Len())

	case *types.Slice:
		return conv.call(rt.FnSliceLen, arg)

	default:
		panic(exn.Conv(conv.fileSet, "can't apply len", arg))
	}
}

func (conv *converter) capBuiltin(arg ast.Expr) sexp.Form {
	switch typ := conv.typeOf(arg).(type) {
	case *types.Array:
		return sexp.Int(typ.Len())

	case *types.Slice:
		return conv.call(rt.FnSliceCap, arg)

	default:
		panic(exn.Conv(conv.fileSet, "can't apply cap", arg))
	}
}

func (conv *converter) makeBuiltin(args []ast.Expr) sexp.Form {
	switch typ := conv.typeOf(args[0]).(type) {
	case *types.Map:
		if len(args) == 2 {
			return conv.call(rt.FnMakeMapCap, args[1])
		}
		return conv.call(rt.FnMakeMap)

	case *types.Slice:
		zv := ZeroValue(typ.Elem())
		if len(args) == 2 {
			return conv.call(rt.FnMakeSlice, args[1], zv)
		}
		return conv.call(rt.FnMakeSliceCap, args[1], args[2], zv)

	default:
		panic(exn.Conv(conv.fileSet, "can't make", args[0]))
	}
}

func (conv *converter) appendBuiltin(args []ast.Expr) sexp.Form {
	if len(args) != 2 {
		panic(exn.NoImpl("variadic append"))
	}

	x := conv.copyValue(conv.Expr(args[1]))
	return conv.call(rt.FnSlicePush, args[0], x)
}
