// Package sexp provides a high level intermediate representation
// that contains both Go and Emacs Lisp traits.
package sexp

import (
	"go/types"
	"lisp/function"
)

type Form interface {
	Type() types.Type
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
	ArrayLit struct {
		Vals []Form
		Typ  *types.Array
	}

	// SparseArrayLit is like ArrayLit, but does not store zero values.
	SparseArrayLit struct {
		Ctor *Call
		Vals []SparseArrayVal
		Typ  *types.Array
	}
)

// SparseArrayVal is SparseArrayLit member initializer.
type SparseArrayVal struct {
	Index int64
	Expr  Form
}

// ArrayIndex is array index expression.
type ArrayIndex struct {
	Array Form
	Index Form
}

// ArrayUpdate is array index expression with assignment.
type ArrayUpdate struct {
	Array Form
	Index Form
	Expr  Form
}

// ArrayCopy used for array copy insertions.
type ArrayCopy struct {
	Array Form
}

type (
	// SliceLen = len(slice).
	SliceLen struct{ Slice Form }
	// SliceCap = cap(slice).
	SliceCap struct{ Slice Form }
)

// SliceIndex is slice index expression.
type SliceIndex struct {
	Slice Form
	Index Form
}

// SliceUpdate is slice index expression with assignment.
type SliceUpdate struct {
	Slice Form
	Index Form
	Expr  Form
}

// Call expression is normal (direct) function invocation.
type Call struct {
	Fn   *function.Type
	Args []Form
}

// CallStmt is a Call which discards returned results.
type CallStmt struct {
	*Call
}

// MultiValueRef is an expression that extracts nth
// multi-value result.
type MultiValueRef struct {
	Index int
	Typ   types.Type
}

// Var - reference to a global or local variable.
type Var struct {
	Name string
	Typ  types.Type
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
	Typ  types.Type
}

// LispTypeAssert is a special case of type assert, it
// operates on unboxed Elisp values.
type LispTypeAssert struct {
	Expr Form
	Typ  types.Type
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
	Body *Block
}

/* Builtin ops */

type (
	BitOr  struct{ Args [2]Form }
	BitAnd struct{ Args [2]Form }
	BitXor struct{ Args [2]Form }

	NumAddX struct {
		Arg Form
		X   int64
		Typ *types.Basic
	}

	NumSubX struct {
		Arg Form
		X   int64
		Typ *types.Basic
	}

	NumAdd struct {
		Args [2]Form
		Typ  *types.Basic
	}
	NumSub struct {
		Args [2]Form
		Typ  *types.Basic
	}
	NumMul struct {
		Args [2]Form
		Typ  *types.Basic
	}
	NumQuo struct {
		Args [2]Form
		Typ  *types.Basic
	}
	NumEq struct {
		Args [2]Form
		Typ  *types.Basic
	}
	NumNotEq struct {
		Args [2]Form
		Typ  *types.Basic
	}
	NumLt struct {
		Args [2]Form
		Typ  *types.Basic
	}
	NumLte struct {
		Args [2]Form
		Typ  *types.Basic
	}
	NumGt struct {
		Args [2]Form
		Typ  *types.Basic
	}
	NumGte struct {
		Args [2]Form
		Typ  *types.Basic
	}

	Concat      struct{ Args []Form }
	StringEq    struct{ Args [2]Form }
	StringNotEq struct{ Args [2]Form }
	StringLt    struct{ Args [2]Form }
	StringLte   struct{ Args [2]Form }
	StringGt    struct{ Args [2]Form }
	StringGte   struct{ Args [2]Form }
)
