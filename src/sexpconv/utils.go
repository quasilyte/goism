package sexpconv

import (
	"go/ast"
	"go/types"
	"sexp"
	"sys_info/function"
	"xtypes"
)

func (conv *converter) exprList(nodes []ast.Expr) []sexp.Form {
	forms := make([]sexp.Form, len(nodes))
	for i, node := range nodes {
		forms[i] = conv.Expr(node)
	}
	return forms
}

func (conv *converter) stmtList(nodes []ast.Stmt) []sexp.Form {
	forms := make([]sexp.Form, len(nodes))
	for i, node := range nodes {
		forms[i] = conv.Stmt(node)
	}
	return forms
}

func (conv *converter) valueCopyList(forms []sexp.Form) []sexp.Form {
	for i := range forms {
		forms[i] = conv.valueCopy(forms[i])
	}
	return forms
}

func (conv *converter) valueCopy(form sexp.Form) sexp.Form {
	typ := form.Type()

	// Copy array.
	if xtypes.IsArray(typ) && !isArrayLit(form) {
		return &sexp.LispCall{
			Fn:   function.CopySequence,
			Args: []sexp.Form{form},
		}
	}

	if typ, ok := typ.Underlying().(*types.Struct); ok && !isStructLit(form) {
		return conv.copyStruct(typ, form)
	}

	return form
}

func isStructLit(form sexp.Form) bool {
	_, ok := form.(*sexp.StructLit)
	return ok
}

func isArrayLit(form sexp.Form) bool {
	_, ok := form.(*sexp.ArrayLit)
	if ok {
		return true
	}
	_, ok = form.(*sexp.SparseArrayLit)
	return ok
}
