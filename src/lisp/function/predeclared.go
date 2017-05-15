package function

// Go.el runtime functions.
var (
	Print   = Type{name: "Go--print"}
	Println = Type{name: "Go--println"}
	Panic   = Type{name: "Go--panic"}
)

// Emacs Lisp builtin functions.
var (
	MakeHashTable = *NewNative("make-hash-table")
	Puthash       = Type{name: "puthash"}
	Gethash       = *NewNative("gethash")

	Intern = Type{name: "intern"}
)
