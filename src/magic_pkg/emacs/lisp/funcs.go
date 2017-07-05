package lisp

import (
	"go/build"
	"io/ioutil"
	"regexp"
)

// Func is an arbitrary Emacs Lisp function.
type Func struct {
	Sym string
}

var (
	// Funcs contains all known Emacs Lisp functions.
	// Each function is unique (pointer comparison can be used).
	Funcs map[string]*Func

	// FFI stores {go sym}->{lisp func} mapping.
	// If   "FFI[x].Sym == Funcs[y].Sym",
	// then "FFI[x] == Funcs[y]" is also valid.
	FFI map[string]*Func
)

var (
	FnCopySequence   = &Func{Sym: "copy-sequence"}
	FnIntern         = &Func{Sym: "intern"}
	FnGethash        = &Func{Sym: "gethash"}
	FnMakeVector     = &Func{Sym: "make-vector"}
	FnRemhash        = &Func{Sym: "remhash"}
	FnHashTableCount = &Func{Sym: "hash-table-count"}
	FnVector         = &Func{Sym: "vector"}

	FnStringBytes = &Func{Sym: "string-bytes"}

	FnSubstr   = &Func{Sym: "substring"}
	FnConcat   = &Func{Sym: "concat"}
	FnNeg      = &Func{Sym: "-"}
	FnAdd1     = &Func{Sym: "1+"}
	FnSub1     = &Func{Sym: "1-"}
	FnMin      = &Func{Sym: "min"}
	FnLen      = &Func{Sym: "length"}
	FnIsStr    = &Func{Sym: "stringp"}
	FnIsInt    = &Func{Sym: "integerp"}
	FnIsSymbol = &Func{Sym: "symbolp"}
	FnList     = &Func{Sym: "list"}

	FnCons   = &Func{Sym: "cons"}
	FnCar    = &Func{Sym: "car"}
	FnCdr    = &Func{Sym: "cdr"}
	FnAref   = &Func{Sym: "aref"}
	FnAset   = &Func{Sym: "aset"}
	FnMemq   = &Func{Sym: "memq"}
	FnMember = &Func{Sym: "member"}
	FnLsh    = &Func{Sym: "lsh"}
	FnLogand = &Func{Sym: "logand"}
	FnLogior = &Func{Sym: "logior"}
	FnLogxor = &Func{Sym: "logxor"}
)

// Operators-like functions.
var (
	FnEq     = &Func{Sym: "eq"}
	FnEqual  = &Func{Sym: "equal"}
	FnNumEq  = &Func{Sym: "="}
	FnNumLt  = &Func{Sym: "<"}
	FnNumGt  = &Func{Sym: ">"}
	FnNumLte = &Func{Sym: "<="}
	FnNumGte = &Func{Sym: ">="}
	FnAdd    = &Func{Sym: "+"}
	FnSub    = &Func{Sym: "-"}
	FnMul    = &Func{Sym: "*"}
	FnQuo    = &Func{Sym: "/"}
	FnStrEq  = &Func{Sym: "string="}
	FnStrLt  = &Func{Sym: "string<"}
	FnStrGt  = &Func{Sym: "string>"}
	FnNot    = &Func{Sym: "not"}
)

// InternFunc creates lisp function with lispSym name.
// Two calls for same symbol return identical object.
func InternFunc(lispSym string) *Func {
	if fn := Funcs[lispSym]; fn != nil {
		return fn
	}
	fn := &Func{Sym: lispSym}
	Funcs[lispSym] = fn
	return fn
}

func initFuncs() error {
	// Fetch all FFI mappings.
	rx := regexp.MustCompile(`//goism:"([^)]*)"->"([^)]*)"\n`)
	code, err := ioutil.ReadFile(build.Default.GOPATH + "/src/emacs/lisp/ffi.go")
	if err != nil {
		return err
	}
	directives := rx.FindAllStringSubmatch(string(code), -1)

	// Fill predeclared funcs.
	Funcs = make(map[string]*Func, 64)
	{
		funcs := []*Func{
			FnSubstr,
			FnConcat,
			FnNeg,
			FnAdd1,
			FnSub1,
			FnMin,
			FnLen,

			FnCons,
			FnCar,
			FnCdr,
			FnAref,
			FnAset,
			FnMemq,
			FnMember,
			FnLsh,
			FnLogand,
			FnLogior,
			FnLogxor,

			FnCopySequence,
			FnIntern,
			FnGethash,
			FnMakeVector,
			FnRemhash,
			FnHashTableCount,
			FnVector,

			FnStringBytes,

			FnEq,
			FnEqual,
			FnNumEq,
			FnNumLt,
			FnNumGt,
			FnNumLte,
			FnNumGte,
			FnAdd,
			FnSub,
			FnMul,
			FnQuo,
			FnStrEq,
			FnStrLt,
			FnStrGt,
			FnNot,
		}
		for _, fn := range funcs {
			Funcs[fn.Sym] = fn
		}
	}

	// Initialie FFI mappings.
	FFI = make(map[string]*Func, len(directives))
	for _, d := range directives {
		goSym, lispSym := d[1], d[2]
		FFI[goSym] = InternFunc(lispSym)
	}

	return nil
}
