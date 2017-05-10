package lisp

// Object is unboxed Emacs Lisp object.
type Object interface {
	object()
}

type (
	// Int <- integerp(x)
	Int int
	// Float <- floatp(x)
	Float float64
	// String <- stringp(x)
	String string
	// Bool <- t (any non-nil = true; nil = false)
	Bool bool
)

// Symbol <- symbolp(x); default value is "nil"
type Symbol struct{ _ int }

// Name return symbol name as string.
func (Symbol) Name() string { return "" }
