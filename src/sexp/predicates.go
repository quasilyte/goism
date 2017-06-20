package sexp

import (
	"lang"
	"xtypes"
)

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

// IsReturning returns true for forms that unconditionally
// return from function.
func IsReturning(form Form) bool {
	found := false
	Walk(form, func(form Form) bool {
		switch form := form.(type) {
		case *Switch:
			if form.DefaultBody == EmptyBlock {
				return false
			}
			return IsReturning(form.DefaultBody)

		case *If, *DoTimes, *While, *Repeat:
			found = false
			return false

		case *Return:
			found = true
			return false

		case *Call:
			found = lang.FuncIsThrowing(form.Fn.Name)
			return !found
		case *LispCall:
			found = lang.FuncIsThrowing(form.Fn.Sym)
			return !found

		default:
			return true
		}
	})
	return found
}
