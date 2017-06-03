package function

import (
	"go/types"
	"lisp"
	"lisp/rt"
)

var (
	emptyTuple    = types.NewTuple()
	nativeResults = tuple(lisp.TypObject)
)

func tuple(typs ...types.Type) *types.Tuple {
	vars := make([]*types.Var, len(typs))
	for i, typ := range typs {
		vars[i] = types.NewVar(0, nil, "", typ)
	}
	return types.NewTuple(vars...)
}

type Fn struct {
	name       string
	results    *types.Tuple
	Complexity int
}

func New(name string, sig *types.Signature) *Fn {
	fn := &Fn{name: name}

	if results := sig.Results(); results != nil {
		fn.results = results
	} else {
		fn.results = emptyTuple
	}

	return fn
}

func (fn *Fn) Name() string {
	return fn.name
}

func (fn *Fn) Results() *types.Tuple {
	return fn.results
}

func (fn *Fn) IsVoid() bool {
	return fn.results.Len() == 0
}

func (fn *Fn) IsPanic() bool {
	return rt.ThrowingFuncs[fn.name]
}
