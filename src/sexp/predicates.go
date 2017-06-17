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
	// #REFS: 49.
	switch form := form.(type) {
	case *Return:
		return true

	case *Block:
		for _, form := range form.Forms {
			if IsReturning(form) {
				return true
			}
		}

	case *If:
		// If both branches return, whole statement returns.
		return IsReturning(form.Then) && IsReturning(form.Else)

	case *ExprStmt:
		return IsReturning(form.Expr)

	case *Call:
		return lang.FuncIsThrowing(form.Fn.Name)
	case *LispCall:
		return lang.FuncIsThrowing(form.Fn.Sym)
	}

	return false
}
