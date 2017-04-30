// Package sexp provides a high level intermediate representation
// that contains both Go and Emacs Lisp traits.
package sexp

type Node interface {
	// This method will be removed when Node will have
	// non-dummy method. Used to avoid arbitrary types
	// being passed as Node.
	private()
}

/* Atoms */

type Bool struct{ Val bool }
type Char struct{ Val rune }
type Int struct{ Val int64 }
type Float struct{ Val float64 }
type String struct{ Val string }
type Var struct{ Name string }

/* Composite literals */

type ArrayLit struct {
	Vals []Node
}

type QuotedArray struct {
	Vals []Node
}

/* Special forms */

// Block can be described as "let-like form",
// but unlike classical Lisp let, it is a statement,
// not expression.
type Block struct {
	// Nodes form block body.
	Nodes []Node
	// Bindings that extend block lexical environment.
	Locals []Binding
}

// If statement evaluates test expression and,
// depending on the result, one of the branches gets
// executed. Else branch is optional.
type If struct {
	Test Node
	Then Node
	Else Node
}

// Return statement exits the function and returns
// one or more values to the caller.
type Return struct {
	Results []Node
}

/* Builtin ops */

type (
	IntAdd       struct{ Args []Node }
	IntSub       struct{ Args []Node }
	IntMul       struct{ Args []Node }
	IntDiv       struct{ Args []Node }
	IntBitOr     struct{ Args []Node }
	IntBitAnd    struct{ Args []Node }
	IntBitXor    struct{ Args []Node }
	IntRem       struct{ Args []Node }
	IntEq        struct{ Args []Node }
	IntNotEq     struct{ Args []Node }
	IntLess      struct{ Args []Node }
	IntLessEq    struct{ Args []Node }
	IntGreater   struct{ Args []Node }
	IntGreaterEq struct{ Args []Node }

	FloatAdd       struct{ Args []Node }
	FloatSub       struct{ Args []Node }
	FloatMul       struct{ Args []Node }
	FloatDiv       struct{ Args []Node }
	FloatEq        struct{ Args []Node }
	FloatNotEq     struct{ Args []Node }
	FloatLess      struct{ Args []Node }
	FloatLessEq    struct{ Args []Node }
	FloatGreater   struct{ Args []Node }
	FloatGreaterEq struct{ Args []Node }

	Concat          struct{ Args []Node }
	StringEq        struct{ Args []Node }
	StringNotEq     struct{ Args []Node }
	StringLess      struct{ Args []Node }
	StringLessEq    struct{ Args []Node }
	StringGreater   struct{ Args []Node }
	StringGreaterEq struct{ Args []Node }
)

/* Call expressions */

// Call expression is normal (direct) function invocation.
type Call struct {
	Fn   string
	Args []Node
}

/* Helper types (not nodes themself) */

// Binding represents named value.
type Binding struct {
	Name string
	Init Node
}
