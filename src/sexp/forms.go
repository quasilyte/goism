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
	Bool bool
	// Int = rune constant or integer literal.
	Int int64
	// Float = floating point literal (of any supported format).
	Float float64
	// String = raw/normal string literal.
	String string
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

	// SliceLit = []T{...}.
	SliceLit struct {
		Vals []Form
		Typ  *types.Slice
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
	// SliceLen = "len(slice)".
	SliceLen struct{ Slice Form }
	// SliceCap = "cap(slice)".
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

// Subslice = "slice[low:high]".
type Subslice struct {
	Slice Form
	Low   Form
	High  Form
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

// Let introduces single binding that is visible to a
// statement or expression. Binding is destroyed after
// wrapped form is evaluated.
type Let struct {
	Bind *Bind

	// Either of these two is set.
	// Let wraps expression OR statement.
	Expr Form
	Stmt Form
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

type (
	// Repeat is the simplest loop, it executes body N times.
	//
	// Note that it is always unrolled. If unrolling is not
	// optimal, optimizer should replace it with While.
	Repeat struct {
		N    int64
		Body *Block
	}

	// DoTimes is like Repeat, but:
	// - N is not necessary a constant.
	// - Has inductive variable inside loop body (Iter).
	DoTimes struct {
		N     Form
		Iter  Var
		Step  Form
		Body  *Block
		Scope *types.Scope
	}

	// While is a general looping construct.
	While struct {
		Cond Form
		Body *Block
	}
)

/* Builtin ops */

type (
	Shl struct {
		Arg Form
		N   Form
	}
	Shr struct {
		Arg Form
		N   Form
	}

	BitOr  struct{ Args [2]Form }
	BitAnd struct{ Args [2]Form }
	BitXor struct{ Args [2]Form }

	Not struct{ Arg Form }
	Neg struct{ Arg Form }

	NumAddX struct {
		Arg Form
		X   int64
	}
	NumSubX struct {
		Arg Form
		X   int64
	}

	NumAdd   struct{ Args [2]Form }
	NumSub   struct{ Args [2]Form }
	NumMul   struct{ Args [2]Form }
	NumQuo   struct{ Args [2]Form }
	NumEq    struct{ Args [2]Form }
	NumNotEq struct{ Args [2]Form }
	NumLt    struct{ Args [2]Form }
	NumLte   struct{ Args [2]Form }
	NumGt    struct{ Args [2]Form }
	NumGte   struct{ Args [2]Form }

	Concat      struct{ Args []Form }
	StringEq    struct{ Args [2]Form }
	StringNotEq struct{ Args [2]Form }
	StringLt    struct{ Args [2]Form }
	StringLte   struct{ Args [2]Form }
	StringGt    struct{ Args [2]Form }
	StringGte   struct{ Args [2]Form }
)
