package rt

import (
	"emacs/lisp"
)

func itabTag(itab lisp.Object) lisp.Object {
	return aref(itab, 0)
}

func itabMethod(itab lisp.Object, n int) lisp.Object {
	return aref(itab, n+1)
}

// Iface - Go interface.
type Iface struct {
	itab lisp.Object
	data lisp.Object
}

// MakeIface returns interface value for given data object.
// Dynamic type is updated accordingly.
func MakeIface(itab lisp.Object, data lisp.Object) *Iface {
	return &Iface{itab: itab, data: data}
}

// IfaceCall0 invokes specified function on given interface object.
//goism:subst
func IfaceCall0(iface *Iface, fnID int) lisp.Object {
	return lisp.DynCall(itabMethod(iface.itab, fnID), iface.data)
}

// IfaceCall1 like IfaceCall0, but for methods with arity=1.
//goism:subst
func IfaceCall1(iface *Iface, fnID int, a1 lisp.Object) lisp.Object {
	return lisp.DynCall(aref(iface.itab, fnID), iface.data, a1)
}

// IfaceCall2 like IfaceCall0, but for methods with arity=2.
//goism:subst
func IfaceCall2(iface *Iface, fnID int, a1, a2 lisp.Object) lisp.Object {
	return lisp.DynCall(aref(iface.itab, fnID), iface.data, a1, a2)
}

// IfaceCall3 like IfaceCall0, but for methods with arity=3.
//goism:subst
func IfaceCall3(iface *Iface, fnID int, a1, a2, a3 lisp.Object) lisp.Object {
	return lisp.DynCall(aref(iface.itab, fnID), iface.data, a1, a2, a3)
}

// IfaceCall4 like IfaceCall0, but for methods with arity=4.
//goism:subst
func IfaceCall4(iface *Iface, fnID int, a1, a2, a3, a4 lisp.Object) lisp.Object {
	return lisp.DynCall(aref(iface.itab, fnID), iface.data, a1, a2, a3, a4)
}
