package sexpconv

import (
	"go/types"
	"sexp"
)

// Given a form and desired type,
// returns either passed form unchanged or
// unsigned expression with correct overflow bits truncation.
func uintElem(form sexp.Form, dstTyp types.Type) sexp.Form {
	typ, ok := dstTyp.(*types.Basic)
	if !ok {
		return form
	}

	switch typ.Kind() {
	case types.Uint8:
		return sexp.NewBitAnd(form, sexp.Int(0xFF))
	case types.Uint16:
		return sexp.NewBitAnd(form, sexp.Int(0xFFFF))
	case types.Uint32, types.Uint:
		return sexp.NewBitAnd(form, sexp.Int(0xFFFFFF))

	case types.Uint64:
		panic("unimplemented")

	default:
		return form
	}
}
