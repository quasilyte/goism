// Package sexp provides a high level intermediate representation
// that contains both Go and Emacs Lisp traits.
package sexp

import (
	"go/types"
)

type Form interface {
	// IsAtom returns true if underlying implementations
	// belongs to atom category.
	//
	// #FIXME: this method can be obsolete.
	// Its currently unused and probably should be
	// as free function if ever needed.
	IsAtom() bool
}

// Var - reference to lexical variable.
// #FIXME: what category does Var belong to?
type Var struct{ Name string }

/* Atoms */

type Bool struct{ Val bool }
type Char struct{ Val rune }
type Int struct{ Val int64 }
type Float struct{ Val float64 }
type String struct{ Val string }

/* Composite literals */

type ArrayLit struct {
	Vals []Form
}

type QuotedArray struct {
	Vals []Form
}

/* Special forms */

// Bind associates name with expression (initializer).
// Binding has lexical scoping.
type Bind struct {
	Name string
	Init Form
}

// FormList packs multiple forms together (like "progn").
type FormList struct {
	Forms []Form
}

// Block is a list of statements.
// Unlike FormList, it creates a new lexical scope.
type Block struct {
	Forms []Form
	Scope *types.Scope
}

// If statement evaluates test expression and,
// depending on the result, one of the branches gets
// executed. Else branch is optional.
type If struct {
	Test Form
	Then *Block
	Else Form
}

// Return statement exits the function and returns
// one or more values to the caller.
type Return struct {
	Results []Form
}

/* Builtin ops */

type (
	IntAdd    struct{ Args []Form }
	IntSub    struct{ Args []Form }
	IntMul    struct{ Args []Form }
	IntDiv    struct{ Args []Form }
	IntBitOr  struct{ Args []Form }
	IntBitAnd struct{ Args []Form }
	IntBitXor struct{ Args []Form }
	IntRem    struct{ Args []Form }
	IntEq     struct{ Args []Form }
	IntNotEq  struct{ Args []Form }
	IntLt     struct{ Args []Form }
	IntLte    struct{ Args []Form }
	IntGt     struct{ Args []Form }
	IntGte    struct{ Args []Form }

	FloatAdd   struct{ Args []Form }
	FloatSub   struct{ Args []Form }
	FloatMul   struct{ Args []Form }
	FloatDiv   struct{ Args []Form }
	FloatEq    struct{ Args []Form }
	FloatNotEq struct{ Args []Form }
	FloatLt    struct{ Args []Form }
	FloatLte   struct{ Args []Form }
	FloatGt    struct{ Args []Form }
	FloatGte   struct{ Args []Form }

	Concat      struct{ Args []Form }
	StringEq    struct{ Args []Form }
	StringNotEq struct{ Args []Form }
	StringLt    struct{ Args []Form }
	StringLte   struct{ Args []Form }
	StringGt    struct{ Args []Form }
	StringGte   struct{ Args []Form }
)

/* Call expressions */

// Call expression is normal (direct) function invocation.
type Call struct {
	Fn   string
	Args []Form
}

/* Helper types (not forms themself) */

// Binding represents named value.
type Binding struct {
	Name string
	Init Form
}
