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

	default:
		panic(fmt.Sprintf("unexpected constant: %#v", cv))
	}
}

func constantChar(cv constant.Value) sexp.Char {
	val, _ := constant.Int64Val(cv)
	return sexp.Char{Val: rune(val)}
}

func constantString(cv constant.Value) sexp.String {
	return sexp.String{Val: constant.StringVal(cv)}
}

func constantInt(cv constant.Value) sexp.Int {
	val, _ := constant.Int64Val(cv)
	return sexp.Int{Val: val}
}

func constantFloat(cv constant.Value) sexp.Float {
	val, _ := constant.Float64Val(cv)
	return sexp.Float{Val: val}
}
