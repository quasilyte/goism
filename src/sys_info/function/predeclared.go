package function

type LispFn struct {
	Sym string
}

var (
	CopySequence   = &LispFn{Sym: "copy-sequence"}
	Intern         = &LispFn{Sym: "intern"}
	Gethash        = &LispFn{Sym: "gethash"}
	MakeVector     = &LispFn{Sym: "make-vector"}
	Remhash        = &LispFn{Sym: "remhash"}
	HashTableCount = &LispFn{Sym: "hash-table-count"}
	Lsh            = &LispFn{Sym: "lsh"}
	Logand         = &LispFn{Sym: "logand"}
	Logior         = &LispFn{Sym: "logior"}
	Logxor         = &LispFn{Sym: "logxor"}
	StrGt          = &LispFn{Sym: "string>"}
	Vector         = &LispFn{Sym: "vector"}
)
