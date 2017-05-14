package compiler

import (
	"lisp/function"
	"sexp"
)

func call(cl *Compiler, fn *function.Type, args ...sexp.Form) {
	compileCall(cl, &sexp.Call{Fn: fn, Args: args})
}
