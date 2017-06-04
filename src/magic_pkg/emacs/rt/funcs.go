package rt

import (
	"exn"
	"sexp"
	"tu"
)

var (
	FnSliceLen *sexp.Func
	FnSliceCap *sexp.Func
	FnSliceGet *sexp.Func
	FnSliceSet *sexp.Func
)

func InitFuncs(env *tu.Env) {
	mustFindFunc := func(name string) *sexp.Func {
		if fn := env.Func(name); fn != nil {
			return fn
		}
		panic(exn.Logic("`emacs/rt' misses `%s' function", name))
	}

	FnSliceLen = mustFindFunc("SliceLen")
	FnSliceCap = mustFindFunc("SliceCap")
	FnSliceGet = mustFindFunc("SliceGet")
	FnSliceSet = mustFindFunc("SliceSet")
}
