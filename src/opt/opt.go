package opt

import (
	"sexp"
)

// OptimizeFunc applies all optimizations on a given function.
func OptimizeFunc(fn *sexp.Func) {
	RemoveDeadCode(fn.Body)
	InlineCalls(fn)
	ReduceStrength(fn.Body)
}
