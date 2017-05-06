package sexpconv

import (
	"go/ast"
	"go/types"
	"sexp"
)

func (conv *Converter) makeBuiltin(args []ast.Expr) sexp.Form {

	switch typ := conv.typeOf(args[0]); typ.(type) {
	case *types.Map:
		if len(args) > 1 {
			return sexp.MakeMap{SizeHint: conv.Expr(args[1])}
		}
		return sexp.MakeMap{SizeHint: sexp.Int{Val: 65}}

	// #TODO: channels and slices.
	default:
		panic("unimplemented")
	}
}
