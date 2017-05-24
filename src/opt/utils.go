package opt

import (
	"sexp"
)

func numEq(form sexp.Form, val int64) bool {
	switch form := form.(type) {
	case sexp.Int:
		return form.Val == val
	case sexp.Float:
		return form.Val == float64(val)
	default:
		return false
	}
}

func addX(arg sexp.Form, x int64) *sexp.NumAddX {
	return &sexp.NumAddX{Arg: arg, X: x}
}

func subX(arg sexp.Form, x int64) *sexp.NumSubX {
	return &sexp.NumSubX{Arg: arg, X: x}
}
