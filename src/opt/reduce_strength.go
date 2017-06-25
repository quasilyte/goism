package opt

import (
	"magic_pkg/emacs/lisp"
	"magic_pkg/emacs/rt"
	"sexp"
	"sexpconv"
)

// ReduceStrength replaces operations with their less expensive
// equivalents.
func ReduceStrength(form sexp.Form) sexp.Form {
	sr := strengthReducer{}
	return sr.rewrite(form)
}

type strengthReducer struct{}

func (sr strengthReducer) rewrite(form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, sr.walkForm)
}

func (sr strengthReducer) walkForm(form sexp.Form) sexp.Form {
	switch form := form.(type) {
	case *sexp.LispCall:
		switch form.Fn {
		case lisp.FnAdd:
			return sr.weakenAdd(form)
		case lisp.FnSub:
			return sr.weakenSub(form)
		case lisp.FnConcat:
			return sr.weakenConcat(form)
		case lisp.FnList:
			return sr.weakenList(form)
		case lisp.FnSubstr:
			return sr.weakenSubstr(form)
		}

	case *sexp.ArrayLit:
		return sr.weakenArrayLit(form)
	case *sexp.SparseArrayLit:
		return sr.weakenSparseArrayLit(form)

	case *sexp.Call:
		if form.Fn == rt.FnBytesToStr {
			return sr.weakenBytesToStr(form)
		}
	}

	return nil
}

func (sr strengthReducer) weakenAdd(form *sexp.LispCall) sexp.Form {
	weaken := func(a, b int) sexp.Form {
		if numEq(form.Args[a], 1) {
			return sexp.NewAdd1(form.Args[b])
		}
		if numEq(form.Args[a], 2) {
			return sexp.NewAdd1(sexp.NewAdd1(form.Args[b]))
		}
		// Addition of negative number = substraction.
		if numEq(form.Args[a], -1) {
			return sexp.NewSub1(form.Args[b])
		}
		if numEq(form.Args[a], -2) {
			return sexp.NewSub1(sexp.NewSub1(form.Args[b]))
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

func (sr strengthReducer) weakenSub(form *sexp.LispCall) sexp.Form {
	if numEq(form.Args[1], 1) {
		return sexp.NewSub1(form.Args[0])
	}
	if numEq(form.Args[1], 2) {
		return sexp.NewSub1(sexp.NewSub1(form.Args[0]))
	}
	// Substraction of negative number = addition.
	if numEq(form.Args[1], -1) {
		return sexp.NewAdd1(form.Args[0])
	}
	if numEq(form.Args[1], -2) {
		return sexp.NewAdd1(sexp.NewAdd1(form.Args[0]))
	}
	return form
}

func (sr strengthReducer) weakenArrayLit(form *sexp.ArrayLit) sexp.Form {
	// #TODO: recognize array where all elements are the same.
	//        Replace with "make-vector" call.
	return form
}

func (sr strengthReducer) weakenSparseArrayLit(form *sexp.SparseArrayLit) sexp.Form {
	toArrayLit := func(form *sexp.SparseArrayLit) *sexp.ArrayLit {
		zv := sexpconv.ZeroValue(form.Typ.Elem())
		vals := make([]sexp.Form, int(form.Typ.Len()))
		for i, val := range form.Vals {
			vals[i] = val
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

func (sr strengthReducer) weakenBytesToStr(form *sexp.Call) sexp.Form {
	if arg, ok := form.Args[0].(*sexp.ArraySlice); ok {
		// It is possible to convert array to string without
		// creating a slice.
		switch arg.Kind() {
		case sexp.SpanWhole:
			return sexp.NewConcat(arg.Array, sexp.Str(""))
		default:
			sub := sexp.NewSubstr(arg.Array, arg.Low, arg.High)
			return sexp.NewConcat(sub, sexp.Str(""))
		}
	}

	return form
}

func (sr strengthReducer) weakenConcat(form *sexp.LispCall) sexp.Form {
	switch len(form.Args) {
	case 0:
		return sexp.Str("")
	case 1:
		return form.Args[0]
	default:
		return form // #REFS: 32
	}
}

func (sr strengthReducer) weakenList(form *sexp.LispCall) sexp.Form {
	if len(form.Args) == 0 {
		return sexp.Symbol{Val: "nil"}
	}
	return form
}

func (sr strengthReducer) weakenSubstr(form *sexp.LispCall) sexp.Form {
	if form.Args[1] == nil && form.Args[2] == nil {
		return form.Args[0]
	}
	return form
}
