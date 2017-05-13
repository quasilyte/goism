// Package sexp provides a high level intermediate representation
// that contains both Go and Emacs Lisp traits.
package sexp

import (
	"go/types"
)

type Form interface {
	form()
}

// Atoms.
type (
	// Bool = true or false literal.
	Bool struct{ Val bool }
	// Int = rune constant or integer literal.
	Int struct{ Val int64 }
	// Float = floating point literal (of any supported format).
	Float struct{ Val float64 }
	// String = raw/normal string literal.
	String struct{ Val string }
	// Symbol = lisp.Symbol literal.
	Symbol struct{ Val string }
)

// Composite literals.
type (
	// ArrayLit = [N]T{...}.
	ArrayLit struct{ Vals []Form }
	// QuotedArray = ArrayLit where each element is constant.
	QuotedArray struct{ Vals []Form }
)

// Call expression is normal (direct) function invocation.
type Call struct {
	Fn   string
	Args []Form
}

// Var - reference to lexical variable.
type Var struct{ Name string }

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
	Cond Form
	Then *Block
	Else Form
}

// Return statement exits the function and returns
// one or more values to the caller.
type Return struct {
	Results []Form
}

type While struct {
	Cond Form
	Body []Form
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

/* Helper types (not forms themself) */
