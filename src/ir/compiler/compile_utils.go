package compiler

import "sexp"

func call(cl *Compiler, name string, args ...sexp.Form) {
	compileCall(cl, name, args)
}
