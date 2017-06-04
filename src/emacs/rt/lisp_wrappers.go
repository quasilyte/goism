package rt

import (
	"emacs/lisp"
)

func not(o lisp.Object) lisp.Bool {
	return lisp.CallBool("not", o)
}

func makeVec(length lisp.Int, init lisp.Object) lisp.Object {
	return lisp.Call("make-vector", length, init)
}

func copyArraySpan(arr lisp.Object, from, to lisp.Int) lisp.Object {
	return lisp.Call("substring", arr, from, to)
}

func copyArrayFrom(arr lisp.Object, from lisp.Int) lisp.Object {
	return lisp.Call("substring", arr, from, lisp.Intern("nil"))
}

func concat2(a, b lisp.Object) lisp.Str {
	return lisp.CallStr("concat", a, b)
}

func arrayConcat(a, b lisp.Object) lisp.Object {
	return lisp.Call("vconcat", a, b)
}

func arrayToStr(arr lisp.Object) lisp.Str {
	return lisp.CallStr("concat", arr, lisp.Str(""))
}

func seqLength(seq lisp.Object) lisp.Int {
	return lisp.CallInt("length", seq)
}

func arrayGet(arr lisp.Object, index lisp.Int) lisp.Object {
	return lisp.Call("aref", arr, index)
}

func arraySet(arr lisp.Object, index lisp.Int, val lisp.Object) {
	lisp.Call("aset", arr, index, val)
}
