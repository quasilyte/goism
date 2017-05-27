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
