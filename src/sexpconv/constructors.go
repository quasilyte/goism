package sexpconv

import (
	"sexp"
)

func _numLt(a, b sexp.Form) *sexp.NumLt {
	return &sexp.NumLt{Args: [2]sexp.Form{a, b}}
}

func _addX(arg sexp.Form, x int64) *sexp.NumAddX {
	return &sexp.NumAddX{Arg: arg, X: x}
}
