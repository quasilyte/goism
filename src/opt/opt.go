package opt

import (
	"sexp"
)

func OptimizeFuncs(funcs []*sexp.Func) {
	for {
		score := runOptPass(funcs)
		if score == 0 {
			return
		}
	}
}

func runOptPass(funcs []*sexp.Func) int {
	score := 0
	for _, fn := range funcs {
		score += optimizeFunc(fn)
	}
	return score
}

func optimizeFunc(fn *sexp.Func) int {
	return InlineCalls(fn) +
		ReduceStrength(fn)
}
