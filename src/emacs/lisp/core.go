package lisp

// Call invokes Emacs Lisp function.
//
// Function existance or signature compatibility is
// not checked during compilation.
// If something is wrong, Emacs will complain during evaluation.
func Call(fn string, args ...interface{}) Object

// Special "interface{}" like type which is used to mark
// Emacs Lisp function arguments that can be literally "anything".
// Unlike "interface{}" it does not wrap non-interface types,
// so Emacs Lisp code can inspect values "as is".
//
// Technically, ""
type any interface{}

// Object is unboxed Emacs Lisp object.
// Go-compatible value can be extracted by
// Object methods.
//
// Analogy can be taken with Go "reflect.Value".
type Object interface {
	// Bool returns underlying bool. Panics when booleanp(val) is false.
	Bool() bool

	// Int returns underlying integer. Panics when integerp(val) is false.
	Int() int

	// Float returns underlying integer. Panics when floatp(val) is false.
	Float() float64

	// String returns underlying integer. Panics when stringp(val) is false.
	String() string

	// Symbol returns underlying symbol. Panics when symbolp(val) is false.
	Symbol() Symbol

	object()
}

// Symbol <- "symbolp(x)".
type Symbol interface {
	symbol()
}

// Intern returns the canonical symbol with specified name.
func Intern(name string) Symbol
