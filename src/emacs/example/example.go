// Package example can be used as installation test
// for goism package.
package example

import (
	"emacs/lisp"
)

// HelloEmacs prints "Hello, world!" using `message' Lisp function.
func HelloEmacs() {
	lisp.Insert("Hello, world!")
}
