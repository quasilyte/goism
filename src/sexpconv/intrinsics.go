package sexpconv

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/types"
	"lisp"
	"sexp"
)

func (conv *Converter) intrinFuncCall(sym string, args []ast.Expr) sexp.Form {
	switch sym {
	case "Int", "Float", "String", "Symbol", "Bool":
		// These types can be constructed only from
		// typed values that does not require any
		// convertion, so we ignore them.
		return conv.Expr(args[0])

	case "CallInt", "CallFloat", "CallString", "CallBool", "CallSymbol":
		fallthrough
	case "Call":
		// #FIXME: non-constant symbols should also be valid.
		fn := constant.StringVal(conv.valueOf(args[0]))
		return &sexp.Call{Fn: fn, Args: conv.exprList(args[1:])}

	case "Intern":
		return conv.intrinIntern(args[0])

	default:
		panic(fmt.Sprintf("invalid intrinsic: %s", sym))
	}
}

func (conv *Converter) intrinMethodCall(typ *types.Named, fn string, args []ast.Expr) sexp.Form {
	if typ == lisp.Types.Symbol {
		switch fn {
		case "Name":
			return conv.call("symbol-name", args...)
		}
	}

	panic("unimplemented")
}

func (conv *Converter) intrinIntern(arg ast.Expr) sexp.Form {
	if cv := conv.valueOf(arg); cv != nil {
		s := constant.StringVal(cv)
		if s == "" {
			return sexp.Symbol{Val: "##"}
		}
		return sexp.Symbol{Val: s}
	}

	return &sexp.Call{Fn: "intern", Args: []sexp.Form{conv.Expr(arg)}}
}
