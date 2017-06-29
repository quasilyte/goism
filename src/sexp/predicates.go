package sexp

import (
	"lang"
	"xtypes"
)

func IsEmptyForm(form Form) bool {
	_, ok := form.(*emptyForm)
	return ok
}

// IsStmt returns true for statement form.
func IsStmt(form Form) bool {
	return form.Type() == xtypes.TypVoid
}

func IsThrow(form Form) bool {
	switch form := form.(type) {
	case *Call:
		return lang.FuncIsThrowing(form.Fn.Name)
	case *LispCall:
		return lang.FuncIsThrowing(form.Fn.Sym)

	default:
		return false
	}
}
