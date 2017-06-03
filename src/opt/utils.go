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

// Inline all sym references with val inside form.
func injectValue(sym string, val, form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, func(form sexp.Form) sexp.Form {
		if form, ok := form.(sexp.Var); ok && form.Name == sym {
			return val
		}
		return nil
	})
}
