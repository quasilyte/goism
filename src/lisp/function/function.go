package function

import (
	"go/types"
	"lisp"
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

func (f *Type) IsVoid() bool {
	return len(f.results) == 0
}

func (f *Type) IsPanic() bool {
	return panicFunctions[f.name]
}

var panicFunctions = map[string]bool{
	"Go--panic": true,
	"error":     true,
	"throw":     true,
	"signal":    true,
}
