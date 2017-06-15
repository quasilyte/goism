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
	methods     map[methodKey]*sexp.Func
}

type funcKey struct {
	pkg  *types.Package
	name string
}

type methodKey struct {
	typ  *types.Named
	name string
}

// NewFuncTable creates initialized function table.
func NewFuncTable(masterPkg *types.Package) *FuncTable {
	return &FuncTable{
		masterPkg:   masterPkg,
		funcs:       make(map[string]*sexp.Func, 32),
		externFuncs: make(map[funcKey]*sexp.Func, 32),
		methods:     make(map[methodKey]*sexp.Func, 32),
	}
}

// MasterPkg returns function table bound package.
func (ftab *FuncTable) MasterPkg() *types.Package {
	return ftab.masterPkg
}

// InsertFunc inserts a new function into table.
func (ftab *FuncTable) InsertFunc(p *types.Package, name string, fn *sexp.Func) {
	if p == ftab.masterPkg {
		ftab.funcs[name] = fn
	} else {
		ftab.externFuncs[funcKey{pkg: p, name: name}] = fn
	}
}

// InsertMethod inserts a new method into table.
func (ftab *FuncTable) InsertMethod(recv *types.Named, name string, fn *sexp.Func) {
	ftab.methods[methodKey{typ: recv, name: name}] = fn
}

// LookupFunc returns stored function or nil if no entry is found.
func (ftab *FuncTable) LookupFunc(p *types.Package, name string) *sexp.Func {
	if p == ftab.masterPkg {
		return ftab.funcs[name]
	}
	return ftab.externFuncs[funcKey{pkg: p, name: name}]
}

// LookupMethod returns stored method or nil if no entry is found.
func (ftab *FuncTable) LookupMethod(recv *types.Named, name string) *sexp.Func {
	return ftab.methods[methodKey{typ: recv, name: name}]
}

// ForEachFunc apply "visit" callback to each stored function.
func (ftab *FuncTable) ForEachFunc(visit func(*sexp.Func)) {
	for _, fn := range ftab.funcs {
		visit(fn)
	}
	for _, fn := range ftab.externFuncs {
		visit(fn)
	}
	for _, fn := range ftab.methods {
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
	for k, fn := range ftab.methods {
		visit(k.typ.Obj().Pkg(), fn)
	}
}
