package sexp

import (
	"vmm"
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
		return vmm.FuncIsThrowing(form.Fn.Name)
	case *LispCall:
		return vmm.FuncIsThrowing(form.Fn.Sym)

	default:
		return false
	}
}
