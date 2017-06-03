package sexpconv

import (
	"exn"
	"go/ast"
	"go/constant"
	"lisp/function"
	"sexp"
)

func (conv *Converter) intrinFuncCall(sym string, args []ast.Expr) sexp.Form {
	switch sym {
	case "Int", "Float", "Str", "Symbol", "Bool":
		// These types can be constructed only from
		// typed values that does not require any
		// convertion, so we ignore them.
		return conv.Expr(args[0])

	case "CallInt", "CallFloat", "CallStr", "CallBool", "CallSymbol":
		fallthrough
	case "Call":
		// #FIXME: non-constant symbols should also be valid.
		name := constant.StringVal(conv.valueOf(args[0]))
		if call := conv.instrCall(name, args[1:]); call != nil {
			return call
		}
		return &sexp.LispCall{
			Fn:   &function.LispFn{Sym: name},
			Args: conv.exprList(args[1:]),
		}

	case "Intern":
		return conv.intrinIntern(args[0])

	default:
		panic(exn.User("`%s' is not a valid intrinsic", sym))
	}
}

func (conv *Converter) intrinIntern(arg ast.Expr) sexp.Form {
	if cv := conv.valueOf(arg); cv != nil {
		s := constant.StringVal(cv)
		if s == "" {
			return sexp.Symbol{Val: "##"}
		}
		return sexp.Symbol{Val: s}
	}

	return conv.lispCall(function.Intern, arg)
}
