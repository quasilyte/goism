package lisp

import (
	"fmt"
	"go/build"
	"io/ioutil"
	"regexp"
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

func initFuncs() error {
	// Fetch all FFI mappings.
	rx := regexp.MustCompile(`//goism:"([^)]*)"->"([^)]*)"\n`)
	code, err := ioutil.ReadFile(build.Default.GOPATH + "/src/emacs/lisp/ffi.go")
	if err != nil {
		return err
	}
	directives := rx.FindAllStringSubmatch(string(code), -1)
	FFI = make(map[string]*Func, len(directives))
	mandatoryFuncs := map[string]**Func{
		"copy-sequence":    &FnCopySequence,
		"intern":           &FnIntern,
		"gethash":          &FnGethash,
		"make-vector":      &FnMakeVector,
		"remhash":          &FnRemhash,
		"hash-table-count": &FnHashTableCount,
		"lsh":              &FnLsh,
		"logand":           &FnLogand,
		"logior":           &FnLogior,
		"logxor":           &FnLogxor,
		"string>":          &FnStrGt,
		"vector":           &FnVector,
	}
	for _, d := range directives {
		from, to := d[1], d[2]
		fn := &Func{Sym: to}
		FFI[from] = fn
		if fnRef, ok := mandatoryFuncs[to]; ok {
			*fnRef = fn
		}
	}

	// Detect missing funcs.
	for sym, fnRef := range mandatoryFuncs {
		if *fnRef == nil {
			return fmt.Errorf("emacs/lisp/ffi.go misses `%s' mapping", sym)
		}
	}

	return nil
}
