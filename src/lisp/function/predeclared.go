package function

// Go.el runtime functions.
var (
	Print   = Type{name: "Go--print"}
	Println = Type{name: "Go--println"}
	Panic   = Type{name: "Go--panic"}

	MakeMap    = *NewNative("Go--make-map")
	MakeMapCap = *NewNative("Go--make-map-cap")

	MapInsert = Type{name: "Go--map-insert"}
)

// Emacs Lisp builtin functions.
var (
	Gethash        = *NewNative("gethash")
	Remhash        = Type{name: "remhash"}
	Vector         = *NewNative("vector")
	Intern         = Type{name: "intern"}
	HashTableCount = *NewNative("hash-table-count")
	CopySequence   = *NewNative("copy-sequence")
	MakeVector     = *NewNative("make-vector")
)
