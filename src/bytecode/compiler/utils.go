package compiler

import (
	"lisp"
	"sexp"
)

func sym(val lisp.Symbol) sexp.Symbol {
	return sexp.Symbol{Val: val}
}
