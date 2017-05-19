package opt

import (
	"sexp"
)

// RemoveDeadCode removes unreachable statements and expressions
// from given sexp form.
func RemoveDeadCode(form sexp.Form) {
	switch form := form.(type) {
	case *sexp.If:
		RemoveDeadCode(form.Then)
		if form.Else != nil {
			RemoveDeadCode(form.Else)
		}

	case *sexp.Block:
		form.Forms = removeDeadCode(form.Forms)
	case *sexp.FormList:
		form.Forms = removeDeadCode(form.Forms)
	case *sexp.While:
		RemoveDeadCode(form.Body)
	}
}

func removeDeadCode(forms []sexp.Form) []sexp.Form {
	for i, form := range forms {
		RemoveDeadCode(form)
		if sexp.IsReturning(form) {
			return forms[:i+1]
		}
	}
	return forms
}
