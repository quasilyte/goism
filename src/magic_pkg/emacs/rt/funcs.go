package rt

import (
	"exn"
	"sexp"
	"tu"
)

var (
	FnPanic *sexp.Func

	FnSlicePush *sexp.Func
	FnSliceLen  *sexp.Func
	FnSliceCap  *sexp.Func
	FnSliceGet  *sexp.Func
	FnSliceSet  *sexp.Func

	FnCoerceBool   *sexp.Func
	FnCoerceInt    *sexp.Func
	FnCoerceFloat  *sexp.Func
	FnCoerceString *sexp.Func
	FnCoerceSymbol *sexp.Func
)

func InitFuncs(env *tu.Env) {
	mustFindFunc := func(name string) *sexp.Func {
		if fn := env.Func(name); fn != nil {
			return fn
		}
		panic(exn.Logic("`emacs/rt' misses `%s' function", name))
	}

	FnPanic = mustFindFunc("Panic")

	FnSliceLen = mustFindFunc("SliceLen")
	FnSliceCap = mustFindFunc("SliceCap")
	FnSliceGet = mustFindFunc("SliceGet")
	FnSliceSet = mustFindFunc("SliceSet")

	FnCoerceBool = mustFindFunc("CoerceBool")
	FnCoerceInt = mustFindFunc("CoerceInt")
	FnCoerceFloat = mustFindFunc("CoerceFloat")
	FnCoerceString = mustFindFunc("CoerceString")
	FnCoerceSymbol = mustFindFunc("CoerceSymbol")
}
