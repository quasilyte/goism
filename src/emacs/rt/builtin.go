package rt

import (
	"emacs/lisp"
)

// Panic triggers run-time panic.
func Panic(errorData lisp.Object) {
	lisp.Call("signal", lisp.Intern("error"), lisp.Call("list", errorData))
}
