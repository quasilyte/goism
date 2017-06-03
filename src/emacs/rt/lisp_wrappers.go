package rt

import (
	"emacs/lisp"
)

func makeVec(length lisp.Int, init lisp.Object) lisp.Object {
	return lisp.Call("make-vector", length, init)
}

func copyArraySpan(data lisp.Object, from, to lisp.Int) lisp.Object {
	return lisp.Call("substring", data, from, to)
}

func concat2(a, b lisp.Object) lisp.Str {
	return lisp.CallStr("contat", a, b)
}
