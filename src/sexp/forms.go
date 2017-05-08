// Package sexp provides a high level intermediate representation
// that contains both Go and Emacs Lisp traits.
package sexp

import (
	"go/types"
	"lisp"
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
type Symbol struct{ Val lisp.Symbol }

/* Composite literals */

type ArrayLit struct {
	Vals []Form
}

type QuotedArray struct {
	Vals []Form
}

/* Special forms */

// Panic causes runtime panic and carries data along.
type Panic struct {
	ErrorData Form
}

// Bind associates name with expression (initializer).
// Binding has lexical scoping.
type Bind struct {
	Name string
	Init Form
}

// Rebind changes symbol value.
type Rebind struct {
	Name string
	Expr Form
}

// TypeAssert coerces expression to specified type; panics on failure.
type TypeAssert struct {
	Expr Form
	Type types.Type
}

// LispTypeAssert is a special case of type assert, it
// operates on unboxed Elisp values.
type LispTypeAssert struct {
	Expr Form
	Type types.Type
}

// ExprStmt represents expression whose result is discarded.
type ExprStmt struct {
	Form Form
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

type MakeMap struct {
	SizeHint Form
}

type MapSet struct {
	Map Form
	Key Form
	Val Form
}

type (
	BitOr  struct{ Args []Form }
	BitAnd struct{ Args []Form }
	BitXor struct{ Args []Form }

	NumAdd struct {
		Args []Form
		Type *types.Basic
	}
	NumSub struct {
		Args []Form
		Type *types.Basic
	}
	NumMul struct {
		Args []Form
		Type *types.Basic
	}
	NumQuo struct {
		Args []Form
		Type *types.Basic
	}
	NumEq struct {
		Args []Form
		Type *types.Basic
	}
	NumNotEq struct {
		Args []Form
		Type *types.Basic
	}
	NumLt struct {
		Args []Form
		Type *types.Basic
	}
	NumLte struct {
		Args []Form
		Type *types.Basic
	}
	NumGt struct {
		Args []Form
		Type *types.Basic
	}
	NumGte struct {
		Args []Form
		Type *types.Basic
	}

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
