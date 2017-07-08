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
	masterFuncs []*sexp.Func
}

type funcKey struct {
	pkg  *types.Package
	name string
}

type methodKey struct {
	pkgName  string
	typeName string
	name     string
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
		ftab.masterFuncs = append(ftab.masterFuncs, fn)
	} else {
		ftab.externFuncs[funcKey{pkg: p, name: name}] = fn
	}
}

// InsertMethod inserts a new method into table.
func (ftab *FuncTable) InsertMethod(recv *types.TypeName, name string, fn *sexp.Func) {
	key := methodKey{
		pkgName:  recv.Pkg().Name(),
		typeName: recv.Name(),
		name:     name,
	}
	ftab.methods[key] = fn
	if key.pkgName == ftab.masterPkg.Name() {
		ftab.masterFuncs = append(ftab.masterFuncs, fn)
	}
}

// LookupFunc returns stored function or nil if no entry is found.
func (ftab *FuncTable) LookupFunc(p *types.Package, name string) *sexp.Func {
	if p == ftab.masterPkg {
		return ftab.funcs[name]
	}
	return ftab.externFuncs[funcKey{pkg: p, name: name}]
}

// LookupMethod returns stored method or nil if no entry is found.
func (ftab *FuncTable) LookupMethod(recv *types.TypeName, name string) *sexp.Func {
	key := methodKey{
		pkgName:  recv.Pkg().Name(),
		typeName: recv.Name(),
		name:     name,
	}
	return ftab.methods[key]
}

// ForEach apply "visit" callback to each stored function.
func (ftab *FuncTable) ForEach(visit func(*sexp.Func)) {
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

func (ftab *FuncTable) MasterFuncs() []*sexp.Func {
	return ftab.masterFuncs
}
