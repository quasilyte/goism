// Package sexp provides a high level intermediate representation
// that contains both Go and Emacs Lisp traits.
package sexp

import (
	"go/types"
	"ir/instr"
	"sys_info/function"
)

// Form = universal S-expression node (akin to Go ast.Node).
type Form interface {
	Type() types.Type
	Copy() Form
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

	/* Str ops */

	OpStrNeq // StrNeq = "X != Y"
	OpStrLte // StrLte = "X <= Y"
	OpStrGt  // StrGt = "X > Y"
	OpStrGte // StrGte = "X >= Y"
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

// Composite literals.
type (
	// ArrayLit = "[N]T{Vals...}".
	ArrayLit struct {
		Vals []Form
		Typ  *types.Array
	}

	// SparseArrayLit is like ArrayLit, but does not store zero values.
	SparseArrayLit struct {
		Ctor Form
		Vals map[int]Form
		Typ  *types.Array
	}

	// SliceLit = "[]T{Vals...}".
	SliceLit struct {
		Vals []Form
		Typ  *types.Slice
	}

	// StructLit is an expression that yields struct object.
	// Each struct member (field) has explicit initializer.
	StructLit struct {
		Vals []Form
		Typ  *types.Struct
	}
)

// Special forms and statements.
type (
	// ArrayUpdate is array index expression with assignment.
	ArrayUpdate struct {
		Array Form
		Index Form
		Expr  Form
	}

	// SliceUpdate is slice index expression with assignment.
	SliceUpdate struct {
		Slice Form
		Index Form
		Expr  Form
	}

	// StructUpdate = "Struct.[Index] = Expr".
	StructUpdate struct {
		Struct Form
		Index  int
		Expr   Form
		Typ    *types.Struct
	}

	// Bind associates name with expression (initializer).
	// Introduces local variable.
	Bind struct {
		Name string
		Init Form
	}

	// Rebind changes local symbol value.
	Rebind struct {
		Name string
		Expr Form
	}

	// VarUpdate changes global variable value.
	VarUpdate struct {
		Name string
		Expr Form
	}

	// FormList packs multiple forms together (like "progn").
	FormList struct{ Forms []Form }

	// Block is a list of statements.
	// Unlike FormList, it creates a new lexical scope.
	Block struct{ Forms []Form }

	// If statement evaluates test expression and,
	// depending on the result, one of the branches gets
	// executed. Else branch is optional.
	If struct {
		Cond Form
		Then *Block
		Else Form
	}

	// Return statement exits the function and returns
	// one or more values to the caller.
	Return struct{ Results []Form }

	// ExprStmt is a Call which discards returned results.
	ExprStmt struct{ Expr Form }
)

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

// Index expressions.
type (
	// ArrayIndex is array index expression.
	ArrayIndex struct {
		Array Form
		Index Form
	}

	// SliceIndex is slice index expression.
	SliceIndex struct {
		Slice Form
		Index Form
	}

	// StructIndex is struct selector expression.
	StructIndex struct {
		Struct Form
		Index  int
		Typ    *types.Struct
	}
)

// Span expressions.
type (
	// ArraySlice is array slicing (subslice) expression.
	ArraySlice struct {
		Array Form
		Typ   *types.Slice
		Span
	}

	// Subslice = "Slice[Low:High]".
	Subslice struct {
		Slice Form
		Span
	}

	// Substr = "Str[Low:High]".
	Substr struct {
		Str Form
		Span
	}
)

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

type LispCall struct {
	Fn   *function.LispFn
	Args []Form
}

// Call expression is normal (direct) function invocation.
type Call struct {
	Fn   *Func
	Args []Form
}

// InstrCall is optimized version of Call/Op.
// Used when there is dedicated opcode available.
type InstrCall struct {
	Instr instr.Instr
	Args  []Form
}

// Let introduces bindings that are visible to a
// statement or expression. Bindings are destroyed after
// wrapped form is evaluated.
type Let struct {
	Bindings []*Bind

	// Either of these two is set.
	// Let wraps expression OR statement.
	Expr Form
	Stmt Form
}

type (
	// And = "X && Y".
	And struct {
		X Form
		Y Form
	}

	// Or = "X || Y".
	Or struct {
		X Form
		Y Form
	}
)
