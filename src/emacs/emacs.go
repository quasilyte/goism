package emacs

/*
	Types
*/

// Object is unboxed Emacs Lisp object.
type Object interface {
	object()
}

type (
	Int    int
	Float  float64
	String string
	Symbol string
)
