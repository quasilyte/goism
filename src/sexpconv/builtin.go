package sexpconv

import (
	"go/ast"
	"go/types"
	"lisp/function"
	"sexp"
)

func (conv *Converter) makeBuiltin(args []ast.Expr) sexp.Form {
	switch typ := conv.typeOf(args[0]); typ.(type) {
	case *types.Map:
		call := &sexp.Call{Fn: &function.MakeHashTable}
		if len(args) == 2 {
			call.Args = []sexp.Form{
				sexp.Symbol{Val: ":size"},
				conv.Expr(args[1]),
				sexp.Symbol{Val: ":test"},
				sexp.Symbol{Val: "equal"},
			}
		} else {
			call.Args = []sexp.Form{
				sexp.Symbol{Val: ":test"},
				sexp.Symbol{Val: "equal"},
			}
		}
		return call

	// #TODO: channels and slices.
	default:
		panic("unimplemented")
	}
}
