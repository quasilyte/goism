package sexpconv

import (
	"go/ast"
	"go/types"
	"lisp/function"
	"sexp"
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
	if isArray(form.Type()) && !isArrayLit(form) {
		return &sexp.Call{
			Fn:   function.CopySequence,
			Args: []sexp.Form{form},
		}
	}
	return form
}

func isGlobal(obj types.Object) bool {
	objScope := obj.Parent()
	// If parent scope is Universe, then object scope
	// is Package => it is global.
	return objScope.Parent() == types.Universe
}

func isArray(typ types.Type) bool {
	_, ok := typ.Underlying().(*types.Array)
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
