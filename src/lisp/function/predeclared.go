package function

import "go/types"

// Go.el runtime functions.
var (
	Print   = &Type{name: "Go--print"}
	Println = &Type{name: "Go--println"}
	Panic   = &Type{name: "Go--panic"}

	MapInsert = &Type{name: "Go--map-insert"}
	SliceCopy = &Type{name: "Go--slice-copy"}
)

// Emacs Lisp builtin functions.
var (
	Gethash        = NewNative("gethash")
	Remhash        = &Type{name: "remhash"}
	Vector         = NewNative("vector")
	Intern         = &Type{name: "intern"}
	HashTableCount = NewNative("hash-table-count")
	CopySequence   = NewNative("copy-sequence")
	MakeVector     = NewNative("make-vector")
	Lsh            = NewNative("lsh")
	Logand         = NewNative("logand")
	Logior         = NewNative("logior")
)

func AppendOne(typ *types.Slice) *Type {
	return &Type{
		name:    "Go--slice-push",
		results: tuple(typ),
	}
}

func MakeMap(typ *types.Map) *Type {
	return &Type{
		name:    "Go--make-map",
		results: tuple(typ),
	}
}

func MakeMapCap(typ *types.Map) *Type {
	return &Type{
		name:    "Go--make-map-cap",
		results: tuple(typ),
	}
}

func MakeSlice(typ *types.Slice) *Type {
	return &Type{
		name:    "Go--make-slice",
		results: tuple(typ),
	}
}

func MakeSliceCap(typ *types.Slice) *Type {
	return &Type{
		name:    "Go--make-slice-cap",
		results: tuple(typ),
	}
}

func MakeSliceFromList(typ *types.Slice) *Type {
	return &Type{
		name:    "Go--make-slice-from-list",
		results: tuple(typ),
	}
}

func Subslice2(typ types.Type) *Type {
	return &Type{
		name:    "Go--subslice2",
		results: tuple(typ),
	}
}

func SubsliceLow(typ types.Type) *Type {
	return &Type{
		name:    "Go--subslice-low",
		results: tuple(typ),
	}
}

func SubsliceHigh(typ types.Type) *Type {
	return &Type{
		name:    "Go--subslice-high",
		results: tuple(typ),
	}
}

func ArraySlice(typ *types.Slice) *Type {
	return &Type{
		name:    "Go--array-slice",
		results: tuple(typ),
	}
}

func ArraySliceWhole(typ *types.Slice) *Type {
	return &Type{
		name:    "Go--array-slice-whole",
		results: tuple(typ),
	}
}

func ArraySliceLow(typ *types.Slice) *Type {
	return &Type{
		name:    "Go--array-slice-low",
		results: tuple(typ),
	}
}

func ArraySliceHigh(typ *types.Slice) *Type {
	return &Type{
		name:    "Go--array-slice-high",
		results: tuple(typ),
	}
}
