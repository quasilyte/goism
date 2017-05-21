package function

import (
	"go/types"
	"lisp"
)

var (
	emptyTuple = types.NewTuple()

	nativeResults = types.NewTuple(
		types.NewVar(0, lisp.Package, "", lisp.Types.Object),
	)
)

type Type struct {
	name    string
	results *types.Tuple
}

func NewNative(name string) *Type {
	return &Type{
		name:    name,
		results: nativeResults,
	}
}

func New(name string, sig *types.Signature) *Type {
	f := &Type{name: name}

	if results := sig.Results(); results != nil {
		f.results = results
	} else {
		f.results = emptyTuple
	}

	return f
}

func (f *Type) Name() string {
	return f.name
}

func (f *Type) Results() types.Type {
	return f.results
}

func (f *Type) IsVoid() bool {
	return f.results.Len() == 0
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
