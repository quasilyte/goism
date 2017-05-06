package sexpconv

import (
	"fmt"
	"go/ast"
	"go/constant"
	"sexp"
)

func (conv *Converter) intrinsic(sym string, args []ast.Expr) *sexp.Call {
	switch sym {
	case "Call":
		// #FIXME: non-constant symbols should also be valid.

		fn := constant.StringVal(conv.valueOf(args[0]))
		return &sexp.Call{Fn: fn, Args: conv.exprList(args[1:])}

	default:
		panic(fmt.Sprintf("invalid intrinsic: %s", sym))
	}
}
