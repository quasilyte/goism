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

type paramKind int

const (
	pkUnused paramKind = iota
	pkUsedOnce
	pkUsedManyTimes
	pkShadowed
	pkAssigned
)

func inspectParam(name string, form sexp.Form) paramKind {
	const (
		shadowed = -1
		assigned = -2
	)
	usages := 0

	sexp.Walk(form, func(form sexp.Form) bool {
		switch form := form.(type) {
		case sexp.Local:
			if form.Name == name {
				usages++
			}

		case *sexp.Bind:
			if form.Name == name {
				usages = shadowed
				return false
			}

		case *sexp.Rebind:
			if form.Name == name {
				usages = assigned
				return false
			}
		}

		return true
	})

	switch usages {
	case shadowed:
		return pkShadowed
	case assigned:
		return pkAssigned
	case 0:
		return pkUnused
	case 1:
		return pkUsedOnce
	default:
		return pkUsedManyTimes
	}
}

// Inline all sym references with val inside form.
func injectValue(name string, val, form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, func(form sexp.Form) sexp.Form {
		local, ok := form.(sexp.Local)
		if !ok {
			return nil
		}
		if local.Name != name {
			return nil
		}
		return val
	})
}
