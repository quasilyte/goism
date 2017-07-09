package rt

import (
	"emacs/lisp"
)

// Iface - Go interface.
type Iface struct {
	dtype lisp.Object
	data  lisp.Object
}

// MakeIface returns interface value for given data object.
// Dynamic type is updated accordingly.
func MakeIface(dtype lisp.Object, data lisp.Object) *Iface {
	return &Iface{dtype: dtype, data: data}
}

// IfaceCall0 invokes specified function on given interface object.
//goism:subst
func IfaceCall0(iface *Iface, fnID int) lisp.Object {
	return lisp.DynCall(aref(iface.dtype, fnID), iface.data)
}

// IfaceCall1 like IfaceCall0, but for methods with arity=1.
//goism:subst
func IfaceCall1(iface *Iface, fnID int, a1 lisp.Object) lisp.Object {
	return lisp.DynCall(aref(iface.dtype, fnID), iface.data, a1)
}

// IfaceCall2 like IfaceCall0, but for methods with arity=2.
//goism:subst
func IfaceCall2(iface *Iface, fnID int, a1, a2 lisp.Object) lisp.Object {
	return lisp.DynCall(aref(iface.dtype, fnID), iface.data, a1, a2)
}

// IfaceCall3 like IfaceCall0, but for methods with arity=3.
//goism:subst
func IfaceCall3(iface *Iface, fnID int, a1, a2, a3 lisp.Object) lisp.Object {
	return lisp.DynCall(aref(iface.dtype, fnID), iface.data, a1, a2, a3)
}

// IfaceCall4 like IfaceCall0, but for methods with arity=4.
//goism:subst
func IfaceCall4(iface *Iface, fnID int, a1, a2, a3, a4 lisp.Object) lisp.Object {
	return lisp.DynCall(aref(iface.dtype, fnID), iface.data, a1, a2, a3, a4)
}
