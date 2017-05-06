package example

import (
	"emacs"
)

func cons(car, cdr emacs.Object) emacs.Object {
	return emacs.Call("cons", car, cdr)
}

func list(xs ...emacs.Object) emacs.Object {
	return emacs.Call("list", xs...)
}

func f() emacs.Object {
	xs := list(1, 2, "3")
	xs = cons("new elem", xs)
	return xs
}
