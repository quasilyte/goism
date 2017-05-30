package sexpconv

import (
	"go/ast"
	"lisp/function"
	"sexp"
	"xtypes"
)

func (conv *Converter) exprList(nodes []ast.Expr) []sexp.Form {
	forms := make([]sexp.Form, len(nodes))
	for i, node := range nodes {
		forms[i] = conv.Expr(node)
	}
	return forms
}

func (conv *Converter) stmtList(nodes []ast.Stmt) []sexp.Form {
	forms := make([]sexp.Form, len(nodes))
	for i, node := range nodes {
		forms[i] = conv.Stmt(node)
	}
	return forms
}

func (conv *Converter) valueCopyList(forms []sexp.Form) []sexp.Form {
	for i := range forms {
		forms[i] = conv.valueCopy(forms[i])
	}
	return forms
}

func (conv *Converter) valueCopy(form sexp.Form) sexp.Form {
	if xtypes.IsArray(form.Type()) && !isArrayLit(form) {
		return &sexp.Call{
			Fn:   function.CopySequence,
			Args: []sexp.Form{form},
		}
	}
	return form
}

func isArrayLit(form sexp.Form) bool {
	_, ok := form.(*sexp.ArrayLit)
	if ok {
		return true
	}
	_, ok = form.(*sexp.SparseArrayLit)
	return ok
}
