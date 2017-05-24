package sexpconv

import (
	"fmt"
	"go/types"
	"lisp"
	"lisp/function"
	"sexp"
)

func ZeroValue(typ types.Type) sexp.Form {
	if types.Identical(typ, lisp.Types.Symbol) {
		return sexp.Symbol{Val: "nil"}
	}

	switch typ := typ.(type) {
	case *types.Basic:
		switch typ.Kind() {
		case types.String:
			return sexp.Str("")
		case types.Bool:
			return sexp.Bool(false)

		default:
			info := typ.Info()

			if info&types.IsFloat != 0 {
				return sexp.Float(0)
			}
			if info&types.IsInteger != 0 {
				return sexp.Int(0)
			}
		}

	case *types.Array:
		ctor := &sexp.Call{
			Fn: function.MakeVector,
			Args: []sexp.Form{
				sexp.Int(typ.Len()),
				ZeroValue(typ.Elem()),
			},
		}
		return &sexp.SparseArrayLit{
			Ctor: ctor,
			Typ:  typ,
		}

	case *types.Map:
		return nilMap
	}

	panic(fmt.Sprintf("can not provide zero value for %#v", typ))
}

// Nil values
var (
	nilMap = sexp.Var{
		Name: "Go--nil-map",
		Typ:  types.NewMap(lisp.Types.Object, lisp.Types.Object),
	}

	nilFunc      = sexp.Var{Name: "Go--nil-function"}
	nilInterface = sexp.Var{Name: "Go--nil-interface"}
	nilSlice     = sexp.Var{Name: "Go--nil-slice"}
)
