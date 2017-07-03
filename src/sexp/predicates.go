package sexp

import (
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
