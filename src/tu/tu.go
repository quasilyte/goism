package tu

import (
	"lisp/function"
	"sexp"
)

// Package contains information about parsed code.
type Package struct {
	Name string

	Funcs []*Func

	// Vars are sorted in order that should be used
	// during initialization.
	Vars []string
	Init *Func

	Env *Env

	Comment string
}

// Func is a Go->sexp converted function.
type Func struct {
	Name     string
	Body     *sexp.Block
	Params   []string
	Variadic bool

	Typ *function.Fn

	DocString string
}
