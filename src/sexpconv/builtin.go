package sexpconv

import (
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
		return &sexp.SliceLen{Slice: conv.Expr(arg)}

	default:
		panic("unimplemented")
	}
}

func (conv *Converter) capBuiltin(arg ast.Expr) sexp.Form {
	switch typ := conv.typeOf(arg).(type) {
	case *types.Array:
		return sexp.Int(typ.Len())

	case *types.Slice:
		return &sexp.SliceCap{Slice: conv.Expr(arg)}

	default:
		panic("unimplemented")
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
		lenArg := conv.Expr(args[1])
		zvArg := ZeroValue(typ.Elem())
		if len(args) == 3 {
			capArg := conv.Expr(args[2])
			return &sexp.Call{
				Fn:   function.MakeSliceCap(typ),
				Args: []sexp.Form{lenArg, capArg, zvArg},
			}
		}
		return &sexp.Call{
			Fn:   function.MakeSlice(typ),
			Args: []sexp.Form{lenArg, zvArg},
		}

	default:
		panic("unimplemented")
	}
}

func (conv *Converter) appendBuiltin(args []ast.Expr) sexp.Form {
	if len(args) != 2 {
		panic("unimplemented")
	}

	typ := conv.typeOf(args[0]).(*types.Slice)
	return conv.call(function.AppendOne(typ), args...)
}
