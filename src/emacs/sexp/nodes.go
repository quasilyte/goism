// Package sexp provides a high level intermediate representation
// that contains both Go and Emacs Lisp traits.
package sexp

import (
	"io"
)

// Node is a interface that every S-expr implements.
type Node interface {
	WriteTo(io.Writer) (int64, error)
}

// Category 1.
// Atoms.

type Int struct {
	Val int64
}

type Float struct {
	Val float64
}

type String struct {
	Val string
}

type Var struct {
	Name string
}

// Category 2.
// Compound literals.

type SliceLit struct {
	Vals []Node
}

// Category 3.
// Special forms.

type Block struct {
	Nodes []Node
}

type If struct {
	Test Node
	Then Node
	Else Node
}

type Bind struct {
	Sym string
	Val Node
}

type Return struct {
	Result Node
}

// Category 4.
// Primitive operations.

type OpType int

const (
	// Int, Float ops:
	OpAdd OpType = iota
	OpSub
	OpMul
	OpDiv
	// Int, Float, String ops:
	OpEq
	OpGt
	OpGte
	OpLt
	OpLte
)

type VariadicOp struct {
	Type OpType
	Args []Node
}

type TakeAddr struct {
	Arg Node
}

type DerefAddr struct {
	Arg Node
}

// Category 5.
// Call expressions.

type Call struct {
	Fn   string
	Args []Node
}
