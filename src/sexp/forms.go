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
	// Str = raw/normal string literal.
	Str string
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

type UnaryOp struct {
	Kind OpKind
	X    Form
}

type BinOp struct {
	Kind OpKind
	Args [2]Form
}

type OpKind int

const (
	OpInvalid OpKind = iota

	/* Num ops */

	OpShl    // Shl = "Args[0] << Args[1]".
	OpShr    // Shr = "Args[0] >> Args[1]".
	OpBitOr  //  BitOr = "Args[0] | Args[1]".
	OpBitAnd // BitAnd = "Args[0] & Args[1]".
	OpBitXor // BitXor = "Args[0] ^ Args[1]".
	OpAdd    // Add = "Args[0] + Args[1]"
	OpSub    // Sub = "Args[0] - Args[1]"
	OpMul    // Mul = "Args[0] * Args[1]"
	OpQuo    // Quo = "Args[0] / Args[1]"
	OpNumEq  // NumEq = "Args[0] == Args[1]"
	OpNumNeq // NumNeq = "Args[0] != Args[1]"
	OpNumLt  // NumLt = "Args[0] < Args[1]"
	OpNumLte // NumLte = "Args[0] <= Args[1]"
	OpNumGt  // NumGt = "Args[0] > Args[1]"
	OpNumGte // NumGte = "Args[0] >= Args[1]"

	/* Str ops */

	OpConcat // Concat = "Args[0] + Args[1]"
	OpStrEq  // StrEq = "Args[0] == Args[1]"
	OpStrNeq // StrNeq = "Args[0] != Args[1]"
	OpStrLt  // StrLt = "Args[0] < Args[1]"
	OpStrLte // StrLte = "Args[0] <= Args[1]"
	OpStrGt  // StrGt = "Args[0] > Args[1]"
	OpStrGte // StrGte = "Args[0] >= Args[1]"

	/* Unary ops */

	OpNot     // Not = "!Arg"
	OpNeg     // Neg = "-Arg"
	OpAdd1    // OpAdd1 = "Arg+1"
	OpAdd2    // OpAdd2 = "Arg+2"
	OpSub1    // OpAdd1 = "Arg-1"
	OpSub2    // OpAdd2 = "Arg-2"
	OpStrCast // StrCast = "string(Arg)"
)
