package rt

import (
	"exn"
	"sexp"
	"tu/symbols"
)

var (
	FnPanic   *sexp.Func
	FnPrint   *sexp.Func
	FnPrintln *sexp.Func

	FnSlicePush *sexp.Func
	FnSliceLen  *sexp.Func
	FnSliceCap  *sexp.Func
	FnSliceGet  *sexp.Func
	FnSliceSet  *sexp.Func

	FnMakeMap    *sexp.Func
	FnMakeMapCap *sexp.Func
	FnMapInsert  *sexp.Func

	FnCoerceBool   *sexp.Func
	FnCoerceInt    *sexp.Func
	FnCoerceFloat  *sexp.Func
	FnCoerceString *sexp.Func
	FnCoerceSymbol *sexp.Func
)

func InitFuncs(env *symbols.Env) {
	mustFindFunc := func(name string) *sexp.Func {
		if fn := env.Func(name); fn != nil {
			return fn
		}
		panic(exn.Logic("`emacs/rt' misses `%s' function", name))
	}

	FnPanic = mustFindFunc("Panic")
	FnPrint = mustFindFunc("Print")
	FnPrintln = mustFindFunc("Println")

	FnSliceLen = mustFindFunc("SliceLen")
	FnSliceCap = mustFindFunc("SliceCap")
	FnSliceGet = mustFindFunc("SliceGet")
	FnSliceSet = mustFindFunc("SliceSet")

	FnMakeMap = mustFindFunc("MakeMap")
	FnMakeMapCap = mustFindFunc("MakeMapCap")
	FnMapInsert = mustFindFunc("MapInsert")

	FnCoerceBool = mustFindFunc("CoerceBool")
	FnCoerceInt = mustFindFunc("CoerceInt")
	FnCoerceFloat = mustFindFunc("CoerceFloat")
	FnCoerceString = mustFindFunc("CoerceString")
	FnCoerceSymbol = mustFindFunc("CoerceSymbol")
}
