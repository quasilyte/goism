package sexpconv

import (
	"go/ast"
	"go/types"
	"lisp/function"
	"sexp"
)

func (conv *Converter) lenBuiltin(arg ast.Expr) sexp.Form {
	switch conv.typeOf(arg).(type) {
	case *types.Map:
		return conv.call(&function.HashTableCount, arg)

	default:
		panic("unimplemented")
	}
}

func (conv *Converter) makeBuiltin(args []ast.Expr) sexp.Form {
	switch typ := conv.typeOf(args[0]); typ.(type) {
	case *types.Map:
		if len(args) == 2 {
			return conv.call(&function.MakeMapCap, args[1])
		}
		return conv.call(&function.MakeMap)

	// #TODO: channels and slices.
	default:
		panic("unimplemented")
	}
}
