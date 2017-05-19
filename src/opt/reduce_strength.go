package opt

import (
	"sexp"
)

// ReduceStrength replaces operations with their less expensive
// equivalents.
func ReduceStrength(form sexp.Form) sexp.Form {
	switch form := form.(type) {
	case *sexp.NumAdd:
		return weakenAdd(form)
	case *sexp.NumSub:
		return weakenSub(form)

	case *sexp.Bind:
		form.Init = ReduceStrength(form.Init)
	case *sexp.Rebind:
		form.Expr = ReduceStrength(form.Expr)
	case *sexp.Block:
		form.Forms = reduceStrength(form.Forms)
	case *sexp.FormList:
		form.Forms = reduceStrength(form.Forms)
	}

	return form
}

func reduceStrength(forms []sexp.Form) []sexp.Form {
	for i, form := range forms {
		forms[i] = ReduceStrength(form)
	}
	return forms
}

func weakenAdd(form *sexp.NumAdd) sexp.Form {
	weaken := func(a, b int) sexp.Form {
		if numEq(form.Args[a], 1) {
			return addX(form.Args[b], 1, form.Type)
		}
		if numEq(form.Args[a], 2) {
			return addX(form.Args[b], 2, form.Type)
		}
		// Addition of negative number = substraction.
		if numEq(form.Args[a], -1) {
			return subX(form.Args[b], 1, form.Type)
		}
		if numEq(form.Args[a], -2) {
			return subX(form.Args[b], 2, form.Type)
		}
		return nil
	}

	if form := weaken(0, 1); form != nil {
		return form
	}
	// Because "+" is commutative, we can try to apply
	// same patterns against other argument.
	if form := weaken(1, 0); form != nil {
		return form
	}
	return form
}

func weakenSub(form *sexp.NumSub) sexp.Form {
	if numEq(form.Args[1], 1) {
		return subX(form.Args[0], 1, form.Type)
	}
	if numEq(form.Args[1], 2) {
		return subX(form.Args[0], 2, form.Type)
	}
	// Substraction of negative number = addition.
	if numEq(form.Args[1], -1) {
		return addX(form.Args[0], 1, form.Type)
	}
	if numEq(form.Args[1], -2) {
		return addX(form.Args[0], 2, form.Type)
	}
	return form
}
