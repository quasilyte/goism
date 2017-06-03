package tu

import (
	"sexp"
)

// Package contains information about parsed code.
type Package struct {
	Name string

	Funcs []*sexp.Func

	// Vars are sorted in order that should be used
	// during initialization.
	Vars []string
	Init *sexp.Func

	Env *Env

	Comment string
}
