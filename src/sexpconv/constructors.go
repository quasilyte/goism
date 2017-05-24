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

func _numLt(a, b sexp.Form) *sexp.NumLt {
	return &sexp.NumLt{Args: [2]sexp.Form{a, b}}
}

func _addX(arg sexp.Form, x int64) *sexp.NumAddX {
	return &sexp.NumAddX{Arg: arg, X: x}
}

func _bitand(a, b sexp.Form) sexp.Form {
	return &sexp.BitAnd{Args: [2]sexp.Form{a, b}}
}
