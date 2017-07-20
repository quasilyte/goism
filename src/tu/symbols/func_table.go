package symbols

import (
	"go/types"
	"sexp"
)

// FuncTable contains all functions that are used in
// a package that is being translated.
type FuncTable struct {
	masterPkg *types.Package

	funcs       map[string]*sexp.Func
	methods     map[methodKey]*sexp.Func
	externFuncs map[funcKey]*sexp.Func
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

// Inserter returns FuncTableInserter that can be used to
// add function table entries.
func (ftab *FuncTable) Inserter() *FuncTableInserter {
	return &FuncTableInserter{ftab: ftab}
}

// MasterPkg returns function table bound package.
func (ftab *FuncTable) MasterPkg() *types.Package {
	return ftab.masterPkg
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

// FuncTableInserter collects functions to fill FunctionTable.
type FuncTableInserter struct {
	ftab *FuncTable

	masterFuncs []*sexp.Func
	otherFuncs  []*sexp.Func
}

// Func inserts a new function into table.
func (ins *FuncTableInserter) Func(p *types.Package, name string, fn *sexp.Func) {
	if p == ins.ftab.masterPkg {
		ins.ftab.funcs[name] = fn
		ins.masterFuncs = append(ins.masterFuncs, fn)
	} else {
		ins.ftab.externFuncs[funcKey{pkg: p, name: name}] = fn
		ins.otherFuncs = append(ins.otherFuncs, fn)
	}
}

// Method inserts a new method into table.
func (ins *FuncTableInserter) Method(recv *types.TypeName, name string, fn *sexp.Func) {
	key := methodKey{
		pkgName:  recv.Pkg().Name(),
		typeName: recv.Name(),
		name:     name,
	}
	ins.ftab.methods[key] = fn
	if key.pkgName == ins.ftab.masterPkg.Name() {
		ins.masterFuncs = append(ins.masterFuncs, fn)
	} else {
		ins.otherFuncs = append(ins.otherFuncs, fn)
	}
}

// GetMasterFuncs returns functions that are defined inside master package.
// Returned slice elements are sorted with in-source declaration order.
func (ins *FuncTableInserter) GetMasterFuncs() []*sexp.Func {
	return ins.masterFuncs
}

// GetAllFuncs returns all functons ever inserted into function table.
// Returned slice elements are sorted with in-source declaration order.
func (ins *FuncTableInserter) GetAllFuncs() []*sexp.Func {
	return append(ins.otherFuncs, ins.masterFuncs...)
}
