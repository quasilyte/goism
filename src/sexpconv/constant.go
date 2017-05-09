package sexpconv

import (
	"fmt"
	"go/constant"
	"sexp"
)

func Constant(cv constant.Value) sexp.Form {
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

func constantChar(cv constant.Value) sexp.Char {
	val, exact := constant.Int64Val(cv)
	if !exact {
		panic("can not handle inexact char") // #REFS: 17.
	}
	return sexp.Char{Val: rune(val)}
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
	val, exact := constant.Float64Val(cv)
	if !exact {
		panic("can not handle inexact float") // #REFS: 17.
	}
	return sexp.Float{Val: val}
}

func constantBool(cv constant.Value) sexp.Bool {
	return sexp.Bool{Val: constant.BoolVal(cv)}
}
