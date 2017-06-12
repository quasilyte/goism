package rt

import "emacs/lisp"

/*
(defun Go--make-map ()
  (make-hash-table :test #'equal))
(defun Go--make-map-cap (cap)
  (make-hash-table :size cap :test #'equal))
*/

// MakeMap creates a new map.
// Default size value is used (which is 65).
func MakeMap() lisp.Object {
	return lisp.Call("make-hash-table",
		lisp.Intern(":test"),
		lisp.Intern("equal"))
}

// MakeMapCap creates a new map of specified initial size.
func MakeMapCap(capacity int) lisp.Object {
	return lisp.Call("make-hash-table",
		lisp.Intern(":size"),
		lisp.Intern(":test"),
		lisp.Intern("equal"),
		capacity)
}

// MapInsert is a simple wrapper aroung "puthash" which
// panics if nil map "m" is used.
func MapInsert(key lisp.Object, val lisp.Object, m lisp.Object) {
	if lisp.Eq(NilMap, m) {
		panic("assignment to entry in nil map")
	}
	lisp.Puthash(key, val, m)
}
