package sexp

import "go/types"

type Func struct {
	Name     string
	Body     *Block
	Params   []string
	Variadic bool

	Results    *types.Tuple
	Complexity int

	DocString string
}
