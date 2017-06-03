package sexp

import (
	"lisp/function"
)

type Func struct {
	Name     string
	Body     *Block
	Params   []string
	Variadic bool

	Typ *function.Fn

	DocString string
}
