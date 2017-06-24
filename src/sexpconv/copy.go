package sexpconv

import (
	"go/types"
	"magic_pkg/emacs/lisp"
	"sexp"
	"xtypes"
)

func (conv *converter) copyStruct(typ *types.Struct, form sexp.Form) *sexp.StructLit {
	vals := make([]sexp.Form, typ.NumFields())
	for i := 0; i < typ.NumFields(); i++ {
		vals[i] = conv.copyValue(&sexp.StructIndex{
			Struct: form,
			Index:  i,
			Typ:    typ,
		})
	}
	return &sexp.StructLit{Vals: vals, Typ: typ}
}

func (conv *converter) copyValuesList(forms []sexp.Form) []sexp.Form {
	for i := range forms {
		forms[i] = conv.copyValue(forms[i])
	}
	return forms
}

func (conv *converter) copyValue(form sexp.Form) sexp.Form {
	typ := form.Type()

	// Copy array.
	if xtypes.IsArray(typ) && !isArrayLit(form) {
		return &sexp.LispCall{
			Fn:   lisp.FnCopySequence,
			Args: []sexp.Form{form},
		}
	}

	if typ, ok := typ.Underlying().(*types.Struct); ok && !isStructLit(form) {
		return conv.copyStruct(typ, form)
	}

	return form
}
