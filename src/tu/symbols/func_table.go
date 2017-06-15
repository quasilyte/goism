package symbols

import (
	"go/types"
	"sexp"
)

// FuncTable contains all functions that are used in
// a package that is being translated.
type FuncTable struct {
	masterPkg   *types.Package
	externFuncs map[funcKey]*sexp.Func
	funcs       map[string]*sexp.Func
}

type funcKey struct {
	pkg  *types.Package
	name string
}

// NewFuncTable creates initialized function table.
func NewFuncTable() *FuncTable {
	return &FuncTable{
		funcs:       make(map[string]*sexp.Func, 32),
		externFuncs: make(map[funcKey]*sexp.Func, 32),
	}
}

// InsertFunc inserts a new function into table.
func (ftab *FuncTable) InsertFunc(p *types.Package, name string, fn *sexp.Func) {
	if p == nil {
		ftab.funcs[name] = fn
	}
	ftab.externFuncs[funcKey{pkg: p, name: name}] = fn
}

// LookupFunc returns stored function or nil if function is not found.
func (ftab *FuncTable) LookupFunc(p *types.Package, name string) *sexp.Func {
	if p == nil {
		return ftab.funcs[name]
	}
	return ftab.externFuncs[funcKey{pkg: p, name: name}]
}

// ForEachFunc apply "visit" callback to each stored function.
func (ftab *FuncTable) ForEachFunc(visit func(*sexp.Func)) {
	for _, fn := range ftab.funcs {
		visit(fn)
	}
	for _, fn := range ftab.externFuncs {
		visit(fn)
	}
}

// ForEach is like ForEachFunc, but also passes function packages
// as a callback argument.
func (ftab *FuncTable) ForEach(visit func(*types.Package, *sexp.Func)) {
	for _, fn := range ftab.funcs {
		visit(ftab.masterPkg, fn)
	}
	for k, fn := range ftab.externFuncs {
		visit(k.pkg, fn)
	}
}
