package emacs

// Symbol is an object with a unique name.
type Symbol string

// Object is an arbitrary object.
// If object == nil, it is "nil" (empty list).
type Object interface{}
