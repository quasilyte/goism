package sexpconv

import (
	"go/types"
	"sexp"
)

func (conv *converter) copyStruct(typ *types.Struct, form sexp.Form) *sexp.StructLit {
	vals := make([]sexp.Form, typ.NumFields())
	for i := 0; i < typ.NumFields(); i++ {
		vals[i] = conv.valueCopy(&sexp.StructIndex{
			Struct: form,
			Index:  i,
			Typ:    typ,
		})
	}
	return &sexp.StructLit{Vals: vals, Typ: typ}
}
