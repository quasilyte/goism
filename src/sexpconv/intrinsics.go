package sexpconv

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/types"
	"sexp"
)

func intrinsic(info *types.Info, sym string, args []ast.Expr) *sexp.Call {
	switch sym {
	case "Funcall":
		// #FIXME: non-constant symbols should also be valid.
		fn := constant.StringVal(info.Types[args[0]].Value)
		return &sexp.Call{Fn: fn, Args: exprList(info, args[1:])}

	default:
		panic(fmt.Sprintf("invalid intrinsic: %s", sym))
	}

	return nil
}
