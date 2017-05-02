package sexpconv

import (
	"go/types"
	"sizes"
)

const (
	kindInt int64 = iota
	kindUint
	kindFloat
	kindComplex
	kindString
	kindBool
)

type kind struct {
	tag  int64
	size sizes.Enum
}

var mappedKinds = [...]kind{
	types.Bool: {tag: kindBool, size: sizes.X8},

	types.Int:   {tag: kindInt, size: sizes.WordSize},
	types.Int8:  {tag: kindInt, size: sizes.X8},
	types.Int16: {tag: kindInt, size: sizes.X16},
	types.Int32: {tag: kindInt, size: sizes.X32},
	types.Int64: {tag: kindInt, size: sizes.X64},

	types.Uint:    {tag: kindUint, size: sizes.WordSize},
	types.Uint8:   {tag: kindUint, size: sizes.X8},
	types.Uint16:  {tag: kindUint, size: sizes.X16},
	types.Uint32:  {tag: kindUint, size: sizes.X32},
	types.Uint64:  {tag: kindUint, size: sizes.X64},
	types.Uintptr: {tag: kindUint, size: sizes.PtrSize},

	types.Float32: {tag: kindFloat, size: sizes.X32},
	types.Float64: {tag: kindFloat, size: sizes.X64},

	types.String: {tag: kindString, size: sizes.X8},
}

func mapKind(typ *types.Basic) kind {
	if typ.Info()&types.IsComplex != 0 {
		panic("unimplemented")
	}

	// #FIXME: this one should be checked and go away ASAP.
	// Upd.: simple case to reproduce is any non-constexpr
	// that involves numeric literals.
	// e.g.: "x == 1".
	if typ.Info()&types.IsUntyped != 0 {
		panic("unimplemented")
	}

	return mappedKinds[typ.Kind()]
}
