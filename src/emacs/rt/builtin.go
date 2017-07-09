package rt

import (
	"emacs/lisp"
)

// Panic triggers run-time panic.
//goism:noinline
func Panic(errorData lisp.Object) {
	lisp.Call("signal", lisp.Intern("error"), lisp.Call("list", errorData))
}

// Print prints all arguments;
// formatting of arguments is implementation-specific.
//goism:noinline
func Print(args lisp.Object) {
	lisp.Call("princ", lisp.MapConcat(lisp.Prin1ToString, args, ""))
}

// Println is like print but prints spaces between arguments
// and a newline at the end.
//goism:noinline
func Println(args lisp.Object) {
	lisp.Call("princ", lisp.MapConcat(lisp.Prin1ToString, args, " "))
	lisp.Call("terpri") // Better than princ("\n") due to spared "\n"
}
