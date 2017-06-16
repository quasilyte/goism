// Package example can be used as installation test
// for goism package.
package example

import (
	"emacs/lisp"
)

// HelloEmacs inserts "Hello, world!" using `insert' Lisp function.
func HelloEmacs() {
	lisp.Insert("Hello, world!")
}
