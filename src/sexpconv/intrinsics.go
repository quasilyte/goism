package sexpconv

import (
	"fmt"
	"go/ast"
	"go/constant"
	"sexp"
)

func (conv *Converter) intrinsic(sym string, args []ast.Expr) sexp.Form {
	switch sym {
	case "Int", "Float", "String", "Symbol", "Bool":
		// These types can be constructed only from
		// typed values that does not require any
		// convertion, so we ignore them.
		return conv.Expr(args[0])

	case "Call":
		// #FIXME: non-constant symbols should also be valid.
		fn := constant.StringVal(conv.valueOf(args[0]))
		return &sexp.Call{Fn: fn, Args: conv.exprList(args[1:])}

	default:
		panic(fmt.Sprintf("invalid intrinsic: %s", sym))
	}
}
