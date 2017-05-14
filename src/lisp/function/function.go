package function

import (
	"go/types"
)

type ResultKind uint8

const (
	// ResultSingle = single return value.
	ResultSingle ResultKind = iota
	// ResultMulti = multiple return values.
	ResultMulti
	// ResultVoid = one value that should be discarded.
	ResultVoid
	// ResultUndefined = return value is undefined.
	ResultUndefined
)

type Type struct {
	name       string
	resultKind ResultKind
}

// func New(name string) *Type {
// 	return &Type{name: name}
// }

func NewLispFunc(name string) *Type {
	return &Type{
		name:       name,
		resultKind: ResultSingle,
	}
}

func New(name string, sig *types.Signature) *Type {
	f := &Type{name: name}

	if results := sig.Results(); results == nil {
		f.resultKind = ResultVoid
	} else if results.Len() == 1 {
		f.resultKind = ResultSingle
	} else {
		f.resultKind = ResultMulti
	}

	return f
}

func (f *Type) Name() string {
	return f.name
}

func (f *Type) ResultKind() ResultKind {
	return f.resultKind
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
