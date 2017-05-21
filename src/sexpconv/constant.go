package sexpconv

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/types"
	"lisp"
	"sexp"
)

func (conv *Converter) Constant(node ast.Expr) sexp.Form {
	if cv := conv.valueOf(node); cv != nil {
		typ := conv.typeOf(node)
		if types.Identical(typ, lisp.Types.Symbol) {
			return sexp.Symbol{Val: constant.StringVal(cv)}
		}

		switch cv.Kind() {
		case constant.Int:
			return constantInt(cv)
		case constant.Float:
			return constantFloat(cv)
		case constant.String:
			return constantString(cv)
		case constant.Bool:
			return constantBool(cv)

		default:
			panic(fmt.Sprintf("unexpected constant: %#v", cv))
		}
	}

	return nil
}

func constantString(cv constant.Value) sexp.String {
	return sexp.String{Val: constant.StringVal(cv)}
}

func constantInt(cv constant.Value) sexp.Int {
	val, exact := constant.Int64Val(cv)
	if !exact {
		panic("can not handle inexact int") // #REFS: 17.
	}
	return sexp.Int{Val: val}
}

func constantFloat(cv constant.Value) sexp.Float {
	val, _ := constant.Float64Val(cv)
	return sexp.Float{Val: val}
}

func constantBool(cv constant.Value) sexp.Bool {
	return sexp.Bool{Val: constant.BoolVal(cv)}
}
