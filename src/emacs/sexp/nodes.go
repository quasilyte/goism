// Package sexp provides a high level intermediate representation
// that contains both Go and Emacs Lisp traits.
package sexp

import (
	"go/types"
	"io"
)

// Node is a interface that every S-expr implements.
type Node interface {
	WriteTo(io.Writer) (int64, error)
	Type() types.Type
}

// Category 1.
// Atoms.

type Bool struct {
	Val bool
}

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
	Typ  types.Type
}

// Category 2.
// Compound literals.

type ArrayLit struct {
	Vals []Node
	Typ  types.Type
}

type QuotedArray struct {
	Vals []Node
	Typ  types.Type
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

type OpKind int

const (
	OpAdd OpKind = iota
	OpSub
	OpMul
	OpDiv

	OpBitOr
	OpBitAnd
	OpBitXor
	OpConcat
	OpRem

	OpEq
	OpNotEq
	OpLess
	OpLessEq
	OpGreater
	OpGreaterEq
)

type Operation struct {
	OpKind OpKind
	Args   []Node
	Typ    *types.Basic
}

// type TakeAddr struct {
// 	Arg Node
// }

// type DerefAddr struct {
// 	Arg Node
// }

// Category 5.
// Call expressions.

type Call struct {
	Fn   string
	Args []Node
	Typ  types.Type
}
