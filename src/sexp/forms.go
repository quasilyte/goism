// Package sexp provides a high level intermediate representation
// that contains both Go and Emacs Lisp traits.
package sexp

import (
	"go/types"
	"lisp/function"
)

// Form = universal S-expression node (akin to Go ast.Node).
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
	// ArrayLit = "[N]T{Vals...}".
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

	// SliceLit = "[]T{Vals...}".
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

// ArraySlice is array slicing (subslice) expression.
type ArraySlice struct {
	Array Form
	Typ   *types.Slice
	Span
}

type (
	// SliceLen = "len(Slice)".
	SliceLen struct{ Slice Form }
	// SliceCap = "cap(Slice)".
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

// Subslice = "Slice[Low:High]".
type Subslice struct {
	Slice Form
	Span
}

// Substr = "Str[Low:High]".
type Substr struct {
	Str Form
	Span
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

// Loop forms.
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

// Unary ops.
type (
	// Not = "!Arg".
	Not struct{ Arg Form }
	// Neg = "-Arg".
	Neg struct{ Arg Form }

	// AddX = "Arg + X".
	AddX struct {
		Arg Form
		X   int64
	}
	// SubX = "Arg - X".
	SubX struct {
		Arg Form
		X   int64
	}

	// StrCast = "string(Arg)".
	StrCast struct{ Arg Form }
)

// Binary ops.
type (
	// Shl = "Args[0] << Args[1]".
	Shl struct{ Args [2]Form }
	// Shr = "Args[0] >> Args[1]".
	Shr struct{ Args [2]Form }

	// BitOr = "Args[0] | Args[1]".
	BitOr struct{ Args [2]Form }
	// BitAnd = "Args[0] & Args[1]".
	BitAnd struct{ Args [2]Form }
	// BitXor = "Args[0] ^ Args[1]".
	BitXor struct{ Args [2]Form }

	// Add = "Args[0] + Args[1]"
	Add struct{ Args [2]Form }
	// Sub = "Args[0] - Args[1]"
	Sub struct{ Args [2]Form }
	// Mul = "Args[0] * Args[1]"
	Mul struct{ Args [2]Form }
	// Quo = "Args[0] / Args[1]"
	Quo struct{ Args [2]Form }
	// NumEq = "Args[0] == Args[1]"
	NumEq struct{ Args [2]Form }
	// NumNotEq = "Args[0] != Args[1]"
	NumNotEq struct{ Args [2]Form }
	// NumLt = "Args[0] < Args[1]"
	NumLt struct{ Args [2]Form }
	// NumLte = "Args[0] <= Args[1]"
	NumLte struct{ Args [2]Form }
	// NumGt = "Args[0] > Args[1]"
	NumGt struct{ Args [2]Form }
	// NumGte = "Args[0] >= Args[1]"
	NumGte struct{ Args [2]Form }

	// StrEq = "Args[0] == Args[1]"
	StrEq struct{ Args [2]Form }
	// StrNotEq = "Args[0] != Args[1]"
	StrNotEq struct{ Args [2]Form }
	// StrLt = "Args[0] < Args[1]"
	StrLt struct{ Args [2]Form }
	// StrLte = "Args[0] <= Args[1]"
	StrLte struct{ Args [2]Form }
	// StrGt = "Args[0] > Args[1]"
	StrGt struct{ Args [2]Form }
	// StrGte = "Args[0] >= Args[1]"
	StrGte struct{ Args [2]Form }
)

// Variadic ops.
type (
	// Concat = "a + b + ...".
	Concat struct{ Args []Form }
)
