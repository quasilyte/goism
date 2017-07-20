package opt

import (
	"sexp"
)

func OptimizeFuncs(funcs []*sexp.Func) {
	for {
		if !runOptPass(funcs) {
			return
		}
	}
}

func runOptPass(funcs []*sexp.Func) bool {
	triggered := false
	for _, fn := range funcs {
		triggered = triggered || optimizeFunc(fn)
	}
	return triggered
}

func optimizeFunc(fn *sexp.Func) bool {
	return InlineCalls(fn) ||
		FoldConstexpr(fn) ||
		ReduceStrength(fn)
}
