package compiler

import (
	"emacs/lisp"
	"sexp"
)

func sym(val lisp.Symbol) sexp.Symbol {
	return sexp.Symbol{Val: val}
}
