package rt

import (
	"emacs/lisp"
)

func arrayToStr(arr lisp.Object) string {
	return lisp.Concat(arr, "")
}

func strToArray(s string) lisp.Object {
	return lisp.Call("vconcat", s)
}

func makeVector(length int, init lisp.Object) lisp.Object {
	return lisp.Call("make-vector", length, init)
}

func vconcat2(a, b lisp.Object) lisp.Object {
	return lisp.Call("vconcat", a, b)
}

func substringFrom(o lisp.Object, from int) lisp.Object {
	return lisp.Call("substring", o, from)
}

func substring(o lisp.Object, from, to int) lisp.Object {
	return lisp.Call("substring", o, from, to)
}

func aref(arr lisp.Object, index int) lisp.Object {
	return lisp.Call("aref", arr, index)
}
