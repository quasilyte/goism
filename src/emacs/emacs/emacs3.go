package emacs

import (
	"emacs/lisp"
)

// By approximation, this file will be usable after
// roadmap-3 will be finished.

// Symbol is lisp.Symbol wrapper.
type Symbol lisp.Symbol

// NewSymbol creates a new lisp.Symbol and wraps it into emacs.Symbol.
func NewSymbol(name string) Symbol {
	return Symbol(lisp.Intern(name))
}

// Name return symbol name as string.
func (sym Symbol) Name() string {
	return string(lisp.CallString("symbol-name", lisp.Symbol(sym)))
}
