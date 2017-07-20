package sexpconv

import (
	"go/types"
	"magic_pkg/emacs/lisp"
	"magic_pkg/emacs/rt"
	"sexp"
	"xtypes"
)

func (conv *converter) copyStruct(typ *types.Named, form sexp.Form) *sexp.StructLit {
	structTyp := typ.Underlying().(*types.Struct)
	vals := make([]sexp.Form, structTyp.NumFields())
	for i := 0; i < structTyp.NumFields(); i++ {
		vals[i] = conv.copyValue(&sexp.StructIndex{
			Struct: form,
			Index:  i,
			Typ:    structTyp,
		}, structTyp.Field(i).Type())
	}
	return &sexp.StructLit{Vals: vals, Typ: typ}
}

func (conv *converter) copyNamed(typ *types.Named, form sexp.Form) sexp.Form {
	if xtypes.IsStruct(typ) && !isStructLit(form) {
		return conv.copyStruct(typ, form)
	}
	// #FIXME: copy interface types?
	return form
}

func (conv *converter) copyValueList(forms []sexp.Form, dstTyp types.Type) {
	for i, form := range forms {
		forms[i] = conv.copyValue(form, dstTyp)
	}
}

func (conv *converter) copyValue(form sexp.Form, dstTyp types.Type) sexp.Form {
	var res sexp.Form
	typ := form.Type()

	if xtypes.IsArray(typ) && !isArrayLit(form) {
		res = sexp.NewLispCall(lisp.FnCopySequence, form) // Copy array.
	} else if typ, ok := typ.(*types.Named); ok {
		res = conv.copyNamed(typ, form)
	} else {
		res = form
	}

	if dstTyp != nil && types.IsInterface(dstTyp) {
		dstTyp := dstTyp.(*types.Named)
		if dstTyp.Obj().Pkg() == lisp.Package || types.Identical(typ, dstTyp) {
			return res
		}
		itab := conv.itabEnv.Intern(xtypes.AsNamedType(typ), dstTyp)
		return sexp.NewCall(
			rt.FnMakeIface,
			sexp.Var{Name: itab, Typ: lisp.TypObject},
			res,
		)
	}
	return res
}
