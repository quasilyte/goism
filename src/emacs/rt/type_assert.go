package rt

import (
	"emacs/lisp"
)

func CoerceBool(x lisp.Object) lisp.Object {
	if lisp.IsBool(x) {
		return x
	}
	panic("interface conversion: lisp.Object is " + objectTypeName(x) + ", not bool")
}

func CoerceInt(x lisp.Object) lisp.Object {
	if lisp.IsInt(x) {
		return x
	}
	panic("interface conversion: lisp.Object is " + objectTypeName(x) + ", not int")
}

func CoerceFloat(x lisp.Object) lisp.Object {
	if lisp.IsFloat(x) {
		return x
	}
	panic("interface conversion: lisp.Object is " + objectTypeName(x) + ", not float64")
}

func CoerceString(x lisp.Object) lisp.Object {
	if lisp.IsString(x) {
		return x
	}
	panic("interface conversion: lisp.Object is " + objectTypeName(x) + ", not string")
}

func CoerceSymbol(x lisp.Object) lisp.Object {
	if lisp.IsSymbol(x) {
		return x
	}
	panic("interface conversion: lisp.Object is " + objectTypeName(x) + ", not lisp.Symbol")
}

func objectTypeName(x lisp.Object) string {
	if lisp.IsString(x) {
		return "string"
	} else if lisp.IsInt(x) {
		return "int"
	} else if lisp.IsSymbol(x) {
		return "lisp.Symbol"
	} else if lisp.IsFloat(x) {
		return "float64"
	} else if lisp.IsBool(x) {
		return "bool"
	}
	panic("unexpected type used in lisp.Object type assertion")
}
