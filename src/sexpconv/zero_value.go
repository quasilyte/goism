package sexpconv

import (
	"fmt"
	"go/types"
	"lisp"
	"sexp"
)

func zeroValue(typ types.Type) sexp.Form {
	if types.Identical(typ, lisp.Types.Symbol) {
		return sexp.Symbol{Val: "nil"}
	}

	if typ, ok := typ.(*types.Basic); ok {
		switch typ.Kind() {
		case types.String:
			return sexp.String{}
		case types.Bool:
			return sexp.Bool{}

		default:
			info := typ.Info()

			if info&types.IsFloat != 0 {
				return sexp.Float{}
			}
			if info&types.IsInteger != 0 {
				return sexp.Int{}
			}
		}
	}

	panic(fmt.Sprintf("can not provide zero value for %s", typ))
}
