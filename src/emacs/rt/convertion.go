package rt

import "emacs/lisp"

// ToBool coerces Emacs Lisp value to canonical boolean
// representation ("nil" or "t" symbol).
func ToBool(o lisp.Object) lisp.Bool {
	return not(not(o))
}
