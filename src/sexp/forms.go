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

type OpKind int

const (
	OpInvalid OpKind = iota

	/* Num ops */

	OpShl    // Shl = "X << Y".
	OpShr    // Shr = "X >> Y".
	OpBitOr  //  BitOr = "X | Y".
	OpBitAnd // BitAnd = "X & Y".
	OpBitXor // BitXor = "X ^ Y".
	OpAdd    // Add = "X + Y"
	OpSub    // Sub = "X - Y"
	OpMul    // Mul = "X * Y"
	OpQuo    // Quo = "X / Y"
	OpNumEq  // NumEq = "X == Y"
	OpNumNeq // NumNeq = "X != Y"
	OpNumLt  // NumLt = "X < Y"
	OpNumLte // NumLte = "X <= Y"
	OpNumGt  // NumGt = "X > Y"
	OpNumGte // NumGte = "X >= Y"

	/* Str ops */

	OpConcat // Concat = "X + Y"
	OpStrEq  // StrEq = "X == Y"
	OpStrNeq // StrNeq = "X != Y"
	OpStrLt  // StrLt = "X < Y"
	OpStrLte // StrLte = "X <= Y"
	OpStrGt  // StrGt = "X > Y"
	OpStrGte // StrGte = "X >= Y"

	/* Unary ops */

	OpNot     // Not = "!X"
	OpNeg     // Neg = "-X"
	OpAdd1    // OpAdd1 = "X+1"
	OpAdd2    // OpAdd2 = "X+2"
	OpSub1    // OpAdd1 = "X-1"
	OpSub2    // OpAdd2 = "X-2"
	OpStrCast // StrCast = "string(X)"
	OpArrayCopy
	OpSliceCap // SliceCap = "cap(X)"
	OpSliceLen // SliceLen = "len(X)"
)

// Atoms.
type (
	// Bool = true or false literal.
	Bool bool
	// Int = rune constant or integer literal.
	Int int64
	// Float = floating point literal (of any supported format).
	Float float64
	// Str = raw/normal string literal.
	Str string
	// Symbol = lisp.Symbol literal.
	Symbol struct{ Val string }
	// Var - reference to a global or local variable.
	Var struct {
		Name string
		Typ  types.Type
	}
)

// Operators.
type (
	// UnaryOp = "op(X)".
	UnaryOp struct {
		Kind OpKind
		X    Form
	}

	// BinOp = "op(X, Y)".
	BinOp struct {
		Kind OpKind
		Args [2]Form
	}
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

// ArraySlice is array slicing (subslice) expression.
type ArraySlice struct {
	Array Form
	Typ   *types.Slice
	Span
}

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
