package opt

import "sexp"

// OptimizeFuncs runs all optimizations available
// on each function given.
func OptimizeFuncs(funcs []*sexp.Func) {
	for _, fn := range funcs {
		RemoveDeadCode(fn.Body)
		InlineCalls(fn)
		ReduceStrength(fn.Body)
	}
}
