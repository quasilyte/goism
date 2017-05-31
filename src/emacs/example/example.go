// Package example can be used as installation test
// for Go.el package.
package example

import (
	"emacs/lisp"
)

// PrintMessage prints given message using `message` Lisp function.
func PrintMessage(msg string) {
	lisp.Call("message", lisp.Str(msg))
}

// PrintFiveLetters prints 5 letters using `println' builtin.
func PrintFiveLetters(letters [5]rune) {
	for i := range letters {
		println(letters[i])
	}
}
