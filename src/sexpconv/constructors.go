package sexpconv

/*
func _it(typ types.Type) sexp.Var {
	return sexp.Var{Name: "_it", Typ: typ}
}

func _let(val sexp.Form, form sexp.Form) *sexp.Let {
	bindings := []*sexp.Bind{&sexp.Bind{Name: "_it", Init: val}}
	if sexp.IsStmt(form) {
		return &sexp.Let{
			Bindings: bindings,
			Stmt:     form,
		}
	}
	return &sexp.Let{
		Bindings: bindings,
		Expr:     form,
	}
}
*/
