package symbols

import (
	"go/types"
	"magic_pkg/emacs/lisp"
	"strings"
)

type Env struct {
	masterPkgName string

	symbols       map[string]string
	externSymbols map[*types.Package]map[string]string
}

func pkgFullName(pkgPath string) string {
	offset := strings.Index(pkgPath, "emacs/") + len("emacs/")
	return pkgPath[offset:]
}

func NewEnv(pkgPath string) *Env {
	return &Env{
		masterPkgName: pkgFullName(pkgPath),
		symbols:       make(map[string]string),
		externSymbols: make(map[*types.Package]map[string]string),
	}
}

func (env *Env) ContainsVar(name string) bool {
	_, ok := env.symbols[name]
	return ok
}

func (env *Env) internVar(bucket map[string]string, pkgPath string, name string) string {
	if sym := bucket[name]; sym != "" {
		return sym
	}
	sym := Mangle(pkgPath, name)
	bucket[name] = sym
	return sym
}

func (env *Env) InternVar(pkg *types.Package, name string) string {
	switch {
	case pkg == nil:
		return env.internVar(env.symbols, env.masterPkgName, name)

	case pkg == lisp.Package:
		return lisp.FFI[name].Sym

	default:
		bucket := env.externSymbols[pkg]
		if bucket == nil {
			bucket = make(map[string]string)
			env.externSymbols[pkg] = bucket
		}

		return env.internVar(bucket, pkg.Path()[len("emacs/"):], name)
	}
}
