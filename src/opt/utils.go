package opt

import (
	"sexp"
)

func numEq(form sexp.Form, val int64) bool {
	switch form := form.(type) {
	case sexp.Int:
		return int64(form) == val
	case sexp.Float:
		return float64(form) == float64(val)

	default:
		return false
	}
}

func addX(arg sexp.Form, x int64) *sexp.AddX {
	return &sexp.AddX{Arg: arg, X: x}
}

func subX(arg sexp.Form, x int64) *sexp.SubX {
	return &sexp.SubX{Arg: arg, X: x}
}
