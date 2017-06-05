package rt

import (
	"emacs/lisp"
)

func CoerceBool(x lisp.Object) lisp.Object {
	if lisp.IsBool(x) {
		return x
	}
	lisp.Error("interface conversion: lisp.Object is %s, not bool",
		objectTypeName(x))
	return nil
}

func CoerceInt(x lisp.Object) lisp.Object {
	if lisp.IsInt(x) {
		return x
	}
	lisp.Error("interface conversion: lisp.Object is %s, not int",
		objectTypeName(x))
	return nil
}

func CoerceFloat(x lisp.Object) lisp.Object {
	if lisp.IsFloat(x) {
		return x
	}
	lisp.Error("interface conversion: lisp.Object is %s, not float64",
		objectTypeName(x))
	return nil
}

func CoerceString(x lisp.Object) lisp.Object {
	if lisp.IsString(x) {
		return x
	}
	lisp.Error("interface conversion: lisp.Object is %s, not string",
		objectTypeName(x))
	return nil
}

func CoerceSymbol(x lisp.Object) lisp.Object {
	if lisp.IsSymbol(x) {
		return x
	}
	lisp.Error("interface conversion: lisp.Object is %s, not lisp.Symbol",
		objectTypeName(x))
	return nil
}

func objectTypeName(x lisp.Object) string {
	if lisp.IsInt(x) {
		return "int"
	} else if lisp.IsFloat(x) {
		return "float64"
	} else if lisp.IsString(x) {
		return "string"
	} else if lisp.IsSymbol(x) {
		return "lisp.Symbol"
	}
	lisp.Error("unexpected type used in lisp.Object type assertion")
	return ""
}
