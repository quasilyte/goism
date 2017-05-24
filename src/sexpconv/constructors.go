package sexpconv

import (
	"go/types"
	"sexp"
)

func _it(typ types.Type) sexp.Var {
	return sexp.Var{Name: "_it", Typ: typ}
}

func _let(val sexp.Form, form sexp.Form) *sexp.Let {
	if sexp.IsStmt(form) {
		return &sexp.Let{
			Bind: &sexp.Bind{Name: "_it", Init: val},
			Stmt: form,
		}
	}
	return &sexp.Let{
		Bind: &sexp.Bind{Name: "_it", Init: val},
		Expr: form,
	}
}
