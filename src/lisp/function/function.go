package function

import (
	"go/types"
	"lisp"
)

type ResultKind uint8

const (
	// ResultSingle = single return value.
	ResultSingle ResultKind = iota
	// ResultMulti = multiple return values.
	ResultMulti
	// ResultVoid = one value that should be discarded.
	ResultVoid
)

type Type struct {
	name    string
	results []types.Type
}

func NewNative(name string) *Type {
	return &Type{
		name:    name,
		results: []types.Type{lisp.Types.Object},
	}
}

func New(name string, sig *types.Signature) *Type {
	f := &Type{name: name}

	if results := sig.Results(); results != nil {
		f.results = make([]types.Type, results.Len())
		for i := 0; i < results.Len(); i++ {
			f.results[i] = results.At(i).Type()
		}
	}

	return f
}

func (f *Type) Name() string {
	return f.name
}

func (f *Type) Results() []types.Type {
	return f.results
}

func (f *Type) ResultKind() ResultKind {
	if f.results == nil {
		return ResultVoid
	}
	if len(f.results) == 1 {
		return ResultSingle
	}
	return ResultMulti
}

func (f *Type) ExitsScope() bool {
	return scopeExitFunctions[f.name]
}

var scopeExitFunctions = map[string]bool{
	"Go--panic": true,
	"error":     true,
	"throw":     true,
	"signal":    true,
}
