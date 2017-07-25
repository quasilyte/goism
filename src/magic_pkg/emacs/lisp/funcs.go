package lisp

import (
	"go/build"
	"io/ioutil"
	"regexp"
)

// Func is an arbitrary Emacs Lisp function.
type Func struct {
	Name string
}

var (
	// Funcs contains all known Emacs Lisp functions.
	// Each function is unique (pointer comparison can be used).
	Funcs map[string]*Func

	// FFI stores {go sym}->{lisp func} mapping.
	// If   "FFI[x].Name == Funcs[y].Name",
	// then "FFI[x] == Funcs[y]" is also valid.
	FFI map[string]*Func
)

var (
	FnError             = &Func{Name: "error"}
	FnSignal            = &Func{Name: "signal"}
	FnThrow             = &Func{Name: "throw"}
	FnMapconcat         = &Func{Name: "mapconcat"}
	FnIsMultibyteString = &Func{Name: "multibyte-string-p"}
	FnPrin1ToString     = &Func{Name: "prin1-to-string"}

	FnCopySequence   = &Func{Name: "copy-sequence"}
	FnIntern         = &Func{Name: "intern"}
	FnGethash        = &Func{Name: "gethash"}
	FnMakeVector     = &Func{Name: "make-vector"}
	FnRemhash        = &Func{Name: "remhash"}
	FnHashTableCount = &Func{Name: "hash-table-count"}
	FnVector         = &Func{Name: "vector"}

	FnStringBytes = &Func{Name: "string-bytes"}

	FnSubstr   = &Func{Name: "substring"}
	FnConcat   = &Func{Name: "concat"}
	FnNeg      = &Func{Name: "-"}
	FnAdd1     = &Func{Name: "1+"}
	FnSub1     = &Func{Name: "1-"}
	FnMin      = &Func{Name: "min"}
	FnLen      = &Func{Name: "length"}
	FnIsStr    = &Func{Name: "stringp"}
	FnIsInt    = &Func{Name: "integerp"}
	FnIsFloat  = &Func{Name: "floatp"}
	FnIsSymbol = &Func{Name: "symbolp"}
	FnIsBool   = &Func{Name: "booleanp"}
	FnList     = &Func{Name: "list"}

	FnCons   = &Func{Name: "cons"}
	FnCar    = &Func{Name: "car"}
	FnCdr    = &Func{Name: "cdr"}
	FnAref   = &Func{Name: "aref"}
	FnAset   = &Func{Name: "aset"}
	FnMemq   = &Func{Name: "memq"}
	FnMember = &Func{Name: "member"}
)

// Operators-like functions.
var (
	FnEq     = &Func{Name: "eq"}
	FnEqual  = &Func{Name: "equal"}
	FnNumEq  = &Func{Name: "="}
	FnNumLt  = &Func{Name: "<"}
	FnNumGt  = &Func{Name: ">"}
	FnNumLte = &Func{Name: "<="}
	FnNumGte = &Func{Name: ">="}
	FnAdd    = &Func{Name: "+"}
	FnSub    = &Func{Name: "-"}
	FnMul    = &Func{Name: "*"}
	FnQuo    = &Func{Name: "/"}
	FnStrEq  = &Func{Name: "string="}
	FnStrLt  = &Func{Name: "string<"}
	FnStrGt  = &Func{Name: "string>"}
	FnNot    = &Func{Name: "not"}
	FnLsh    = &Func{Name: "lsh"}    // "<<"
	FnLogand = &Func{Name: "logand"} // "&"
	FnLogior = &Func{Name: "logior"} // "|"
	FnLogxor = &Func{Name: "logxor"} // "^"
)

// InternFunc creates lisp function with lispSym name.
// Two calls for same symbol return identical object.
func InternFunc(lispSym string) *Func {
	if fn := Funcs[lispSym]; fn != nil {
		return fn
	}
	fn := &Func{Name: lispSym}
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
			FnError,
			FnSignal,
			FnThrow,
			FnMapconcat,
			FnIsMultibyteString,
			FnPrin1ToString,
			FnCopySequence,
			FnIntern,
			FnGethash,
			FnMakeVector,
			FnRemhash,
			FnHashTableCount,
			FnVector,
			FnStringBytes,
			FnSubstr,
			FnConcat,
			FnNeg,
			FnAdd1,
			FnSub1,
			FnMin,
			FnLen,
			FnIsStr,
			FnIsInt,
			FnIsFloat,
			FnIsSymbol,
			FnIsBool,
			FnList,
			FnCons,
			FnCar,
			FnCdr,
			FnAref,
			FnAset,
			FnMemq,
			FnMember,
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
			FnLsh,
			FnLogand,
			FnLogior,
			FnLogxor,
		}
		for _, fn := range funcs {
			Funcs[fn.Name] = fn
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
