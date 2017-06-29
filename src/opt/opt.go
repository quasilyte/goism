package opt

import (
	"sexp"
)

// OptimizeFunc applies all optimizations on a given function.
func OptimizeFunc(fn *sexp.Func) {
	InlineCalls(fn)
	ReduceStrength(fn.Body)
}
