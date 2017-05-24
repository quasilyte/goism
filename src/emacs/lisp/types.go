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
	// Str <- stringp(x)
	Str string
	// Bool <- t (any non-nil = true; nil = false)
	Bool bool
)

// Symbol <- symbolp(x); default value is "nil"
type Symbol struct{ _ int }
