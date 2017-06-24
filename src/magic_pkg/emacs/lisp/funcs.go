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
	FnCopySequence   *Func
	FnIntern         *Func
	FnGethash        *Func
	FnMakeVector     *Func
	FnRemhash        *Func
	FnHashTableCount *Func
	FnLsh            *Func
	FnLogand         *Func
	FnLogior         *Func
	FnLogxor         *Func
	FnStrGt          *Func
	FnVector         *Func
)

// Operators-like functions.
var (
	FnNumEq  = &Func{Sym: "="}
	FnNumGt  = &Func{Sym: ">"}
	FnNumLt  = &Func{Sym: "<"}
	FnNumLte = &Func{Sym: "<="}
	FnNumGte = &Func{Sym: ">="}
	FnNumAdd = &Func{Sym: "+"}
	FnNumSub = &Func{Sym: "-"}
	FnNumMul = &Func{Sym: "*"}
	FnNumQuo = &Func{Sym: "/"}
	FnStrEq  = &Func{Sym: "string="}
	FnStrLt  = &Func{Sym: "string<"}
	FnNot    = &Func{Sym: "!"}
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
			FnNumEq,
			FnNumGt,
			FnNumLt,
			FnNumLte,
			FnNumGte,
			FnNumAdd,
			FnNumSub,
			FnNumMul,
			FnNumQuo,
			FnStrEq,
			FnStrLt,
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
