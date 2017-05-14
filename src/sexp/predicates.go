package sexp

// IsReturning returns true for forms that unconditionally
// return from function.
func IsReturning(form Form) bool {
	switch form := form.(type) {
	case *Panic:
		return true

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

	case *While:
		return IsReturning(form.Body)

	case CallStmt:
		return form.Fn.ExitsScope()

	case *Call:
		return form.Fn.ExitsScope()
	}

	return false
}
