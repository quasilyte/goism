package rt

import (
	"emacs/lisp"
)

// Panic triggers run-time panic.
func Panic(errorData lisp.Object) {
	lisp.Call("signal", lisp.Intern("error"), lisp.Call("list", errorData))
}

/*
func Print(args lisp.Object) {
	lisp.Princ(lisp.MapConcat(lisp.Prin1ToString, args, ""))
}

func Println(args lisp.Object) {
	lisp.Princ(lisp.MapConcat(lisp.Prin1ToString, args, " "))
	lisp.Princ("\n")
}
*/
