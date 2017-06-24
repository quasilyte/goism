package lisp

// Should be moved to FFI map.
var (
	FnCopySequence   = &Func{Sym: "copy-sequence"}
	FnIntern         = &Func{Sym: "intern"}
	FnGethash        = &Func{Sym: "gethash"}
	FnMakeVector     = &Func{Sym: "make-vector"}
	FnRemhash        = &Func{Sym: "remhash"}
	FnHashTableCount = &Func{Sym: "hash-table-count"}
	FnLsh            = &Func{Sym: "lsh"}
	FnLogand         = &Func{Sym: "logand"}
	FnLogior         = &Func{Sym: "logior"}
	FnLogxor         = &Func{Sym: "logxor"}
	FnStrGt          = &Func{Sym: "string>"}
	FnVector         = &Func{Sym: "vector"}
)
