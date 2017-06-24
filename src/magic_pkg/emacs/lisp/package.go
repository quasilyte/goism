package lisp

import (
	"go/build"
	"go/types"
	"io/ioutil"
	"regexp"
)

var Package *types.Package

var (
	TypObject *types.Named
	TypSymbol *types.Named
)

var FFI map[string]*Func

func InitPackage(pkg *types.Package) error {
	top := pkg.Scope()
	getNamed := func(name string) *types.Named {
		return top.Lookup(name).(*types.TypeName).Type().(*types.Named)
	}

	Package = pkg

	TypObject = getNamed("Object")
	TypSymbol = getNamed("Symbol")

	// Fetch all FFI mappings.
	rx := regexp.MustCompile(`//goism:"([^)]*)"->"([^)]*)"\n`)
	code, err := ioutil.ReadFile(build.Default.GOPATH + "/src/emacs/lisp/ffi.go")
	if err != nil {
		return err
	}
	directives := rx.FindAllStringSubmatch(string(code), -1)
	FFI = make(map[string]*Func, len(directives))
	for _, d := range directives {
		from, to := d[1], d[2]
		FFI[from] = &Func{Sym: to}
	}

	return nil
}
