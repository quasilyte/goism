package rt

import (
	"exn"
	"fmt"
	"sexp"
	"tu/symbols"
)

var FnIfaceCall [5]*sexp.Func

var (
	FnMakeIface *sexp.Func

	FnPanic   *sexp.Func
	FnPrint   *sexp.Func
	FnPrintln *sexp.Func

	FnMakeSlice      *sexp.Func
	FnMakeSliceCap   *sexp.Func
	FnSliceCopy      *sexp.Func
	FnSlicePush      *sexp.Func
	FnSliceLen       *sexp.Func
	FnSliceCap       *sexp.Func
	FnSliceGet       *sexp.Func
	FnSliceSet       *sexp.Func
	FnSliceSlice2    *sexp.Func
	FnSliceSliceLow  *sexp.Func
	FnSliceSliceHigh *sexp.Func
	FnArrayToSlice   *sexp.Func
	FnArraySlice2    *sexp.Func
	FnArraySliceLow  *sexp.Func
	FnArraySliceHigh *sexp.Func

	FnStringGet *sexp.Func

	FnBytesToStr *sexp.Func
	FnStrToBytes *sexp.Func

	FnMakeMap    *sexp.Func
	FnMakeMapCap *sexp.Func
	FnMapInsert  *sexp.Func

	FnCoerceBool   *sexp.Func
	FnCoerceInt    *sexp.Func
	FnCoerceFloat  *sexp.Func
	FnCoerceString *sexp.Func
	FnCoerceSymbol *sexp.Func
)

func InitFuncs(ftab *symbols.FuncTable) {
	mustFindFunc := func(name string) *sexp.Func {
		if fn := ftab.LookupFunc(Package, name); fn != nil {
			return fn
		}
		panic(exn.Logic("`emacs/rt' misses `%s' function", name))
	}

	for i := range FnIfaceCall {
		FnIfaceCall[i] = mustFindFunc(fmt.Sprintf("IfaceCall%d", i))
	}

	FnMakeIface = mustFindFunc("MakeIface")

	FnPanic = mustFindFunc("Panic")
	FnPrint = mustFindFunc("Print")
	FnPrintln = mustFindFunc("Println")

	FnMakeSlice = mustFindFunc("MakeSlice")
	FnMakeSliceCap = mustFindFunc("MakeSliceCap")
	FnSliceCopy = mustFindFunc("SliceCopy")
	FnSlicePush = mustFindFunc("SlicePush")
	FnSliceLen = mustFindFunc("SliceLen")
	FnSliceCap = mustFindFunc("SliceCap")
	FnSliceGet = mustFindFunc("SliceGet")
	FnSliceSet = mustFindFunc("SliceSet")
	FnSliceSlice2 = mustFindFunc("SliceSlice2")
	FnSliceSliceLow = mustFindFunc("SliceSliceLow")
	FnSliceSliceHigh = mustFindFunc("SliceSliceHigh")
	FnArrayToSlice = mustFindFunc("ArrayToSlice")
	FnArraySlice2 = mustFindFunc("ArraySlice2")
	FnArraySliceLow = mustFindFunc("ArraySliceLow")
	FnArraySliceHigh = mustFindFunc("ArraySliceHigh")

	FnStringGet = mustFindFunc("StringGet")

	FnBytesToStr = mustFindFunc("BytesToStr")
	FnStrToBytes = mustFindFunc("StrToBytes")

	FnMakeMap = mustFindFunc("MakeMap")
	FnMakeMapCap = mustFindFunc("MakeMapCap")
	FnMapInsert = mustFindFunc("MapInsert")

	FnCoerceBool = mustFindFunc("CoerceBool")
	FnCoerceInt = mustFindFunc("CoerceInt")
	FnCoerceFloat = mustFindFunc("CoerceFloat")
	FnCoerceString = mustFindFunc("CoerceString")
	FnCoerceSymbol = mustFindFunc("CoerceSymbol")
}
