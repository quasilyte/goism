package opt

import (
	"sexp"
	"sexpconv"
)

// ReduceStrength replaces operations with their less expensive
// equivalents.
func ReduceStrength(form sexp.Form) sexp.Form {
	switch form := form.(type) {
	case *sexp.Add:
		return weakenAdd(form)
	case *sexp.Sub:
		return weakenSub(form)

	case *sexp.Bind:
		form.Init = ReduceStrength(form.Init)
	case *sexp.Rebind:
		form.Expr = ReduceStrength(form.Expr)
	case *sexp.Block:
		form.Forms = reduceStrength(form.Forms)
	case *sexp.FormList:
		form.Forms = reduceStrength(form.Forms)

	case *sexp.While:
		form.Cond = ReduceStrength(form.Cond)
		ReduceStrength(form.Body)

	case *sexp.ArrayLit:
		return weakenArrayLit(form)
	case *sexp.SparseArrayLit:
		return weakenSparseArrayLit(form)
	}

	return form
}

func reduceStrength(forms []sexp.Form) []sexp.Form {
	for i, form := range forms {
		forms[i] = ReduceStrength(form)
	}
	return forms
}

func weakenAdd(form *sexp.Add) sexp.Form {
	weaken := func(a, b int) sexp.Form {
		if numEq(form.Args[a], 1) {
			return addX(form.Args[b], 1)
		}
		if numEq(form.Args[a], 2) {
			return addX(form.Args[b], 2)
		}
		// Addition of negative number = substraction.
		if numEq(form.Args[a], -1) {
			return subX(form.Args[b], 1)
		}
		if numEq(form.Args[a], -2) {
			return subX(form.Args[b], 2)
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

func weakenSub(form *sexp.Sub) sexp.Form {
	if numEq(form.Args[1], 1) {
		return subX(form.Args[0], 1)
	}
	if numEq(form.Args[1], 2) {
		return subX(form.Args[0], 2)
	}
	// Substraction of negative number = addition.
	if numEq(form.Args[1], -1) {
		return addX(form.Args[0], 1)
	}
	if numEq(form.Args[1], -2) {
		return addX(form.Args[0], 2)
	}
	return form
}

func weakenArrayLit(form *sexp.ArrayLit) sexp.Form {
	// #TODO: recognize array where all elements are the same.
	//        Replace with "make-vector" call.
	return form
}

func weakenSparseArrayLit(form *sexp.SparseArrayLit) sexp.Form {
	toArrayLit := func(form *sexp.SparseArrayLit) *sexp.ArrayLit {
		zv := sexpconv.ZeroValue(form.Typ.Elem())
		vals := make([]sexp.Form, int(form.Typ.Len()))
		for _, val := range form.Vals {
			vals[val.Index] = val.Expr
		}
		for i := range vals {
			if vals[i] == nil {
				vals[i] = zv
			}
		}
		return &sexp.ArrayLit{Vals: vals}
	}

	// If array is very small, it is better to use "vector".
	if form.Typ.Len() <= 4 {
		return toArrayLit(form)
	}

	// Sparse arrays worth it only when zero values are prevalent.
	zeroVals := form.Typ.Len() - int64(len(form.Vals))
	if zeroVals*4 < form.Typ.Len()*3 {
		// Count of zero values < 75%.
		// Convert to ArrayLit.
		return toArrayLit(form)
	}

	return form
}
