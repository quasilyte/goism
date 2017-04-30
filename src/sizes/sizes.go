package sizes

import (
	"fmt"
	"go/types"
	"unsafe"
)

// Enum represents size value.
// It can be coerced to bit or byte size.
type Enum int

// #FIXME: should use sizes of host machine.
const (
	wordSize = int(unsafe.Sizeof(int(0)))
	ptrSize  = int(unsafe.Sizeof(uintptr(0)))
)

// Enumeration of possible integral sizes.
const (
	X8 Enum = iota
	X16
	X32
	X64
	WordSize
	PtrSize
)

// NumBytes gives byte size as int
func (e Enum) NumBytes() int {
	switch e {
	case X8:
		return 1
	case X16:
		return 2
	case X32:
		return 4
	case X64:
		return 8
	case WordSize:
		return wordSize
	case PtrSize:
		return ptrSize

	default:
		panic(fmt.Sprintf("unexpected enum value"))
	}
}

// NumBits gives bit size as int
func (e Enum) NumBits() int {
	return e.NumBytes() * 8
}

func (e Enum) String() string {
	switch e {
	case X8:
		return "8bit"
	case X16:
		return "16bit"
	case X32:
		return "32bit"
	case X64:
		return "64bit"
	case WordSize:
		return "WordSize"
	case PtrSize:
		return "PtrSize"

	default:
		panic(fmt.Sprintf("unexpected enum value"))
	}
}

// Note: does not contain "rune" and "byte"
// because they are aliases.
var bitSizes = [...]Enum{
	types.Uintptr: PtrSize,

	types.Int:  WordSize,
	types.Uint: WordSize,

	types.Int8:  X8,
	types.Uint8: X8,

	types.Int16:  X16,
	types.Uint16: X16,

	types.Int32:  X32,
	types.Uint32: X32,

	types.Int64:  X64,
	types.Uint64: X64,
}

// New creates size enumeration depending on the given type.
func New(typ *types.Basic) Enum {
	return bitSizes[typ.Kind()]
}
