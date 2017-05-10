// Package emacs provides a set of utilities that
// simplify Emacs interop.
// It wraps most common functions invoked with lisp.Call
// in a typesafe interface.
package emacs

// By approximation, this file will be usable after
// roadmap-1 will be finished.

import (
	"emacs/lisp"
)

func Insert(arg string) {
	lisp.Call("insert", lisp.String(arg))
}
