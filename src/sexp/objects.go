package sexp

import "go/types"

type funcInfo uint32

const (
	funcInlineable funcInfo = 1 << iota
)

// IsInlineable tells if function can be inlined
// (not the same as *should* be inlined).
func (fn *Func) IsInlineable() bool { return (fn.info & funcInlineable) != 0 }

// MarkInlineable sets function inlineable flag to true.
func (fn *Func) MarkInlineable() { fn.info |= funcInlineable }

type Func struct {
	Name     string
	Body     Block
	Params   []string
	Variadic bool

	Results *types.Tuple
	info    funcInfo

	DocString string
}
