package sexpconv

import (
	"go/ast"
	"go/types"
	"elapc/instr"
	"sexp"
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

func toFormList(form sexp.Form) *sexp.FormList {
	if form, ok := form.(*sexp.FormList); ok {
		return form
	}
	return &sexp.FormList{Forms: []sexp.Form{form}}
}

// Returns a form which is a equallity comparator for two given forms.
// Returns nil when comparison over {"a", "b"} is undefined (or unimplemented).
func comparatorEq(a, b sexp.Form) sexp.Form {
	switch typ := a.Type(); typ := typ.(type) {
	case *types.Basic:
		if typ.Info()&types.IsNumeric != 0 {
			return sexp.NewNumEq(a, b)
		} else if typ.Kind() == types.String {
			return sexp.NewStrEq(a, b)
		}
		return nil

	case *types.Named:
		// #REFS: 60.
		return nil

	default:
		// Fallback to "eq" comparison.
		// Should work for pointer comparisons.
		return &sexp.InstrCall{
			Instr: instr.Eq,
			Args:  []sexp.Form{a, b},
		}
	}
}
