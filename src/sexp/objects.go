package sexp

import "go/types"

type funcInfo uint32

const (
	funcInlineable funcInfo = 1 << iota
	funcSubst
	funcNoinline
)

// IsInlineable tells if function can be inlined
// (not the same as *should* be inlined).
func (fn *Func) IsInlineable() bool { return (fn.info & funcInlineable) != 0 }

// IsSubst tells if function is "always inline".
func (fn *Func) IsSubst() bool { return (fn.info & funcSubst) != 0 }

// IsNoinline returns true for functions that should not be inlined. Ever.
func (fn *Func) IsNoinline() bool { return (fn.info & funcNoinline) != 0 }

// SetInlineable sets function inlineable flag to true or false.
func (fn *Func) SetInlineable(inlineable bool) {
	if inlineable {
		fn.info |= funcInlineable
	} else {
		fn.info &^= funcInlineable
	}
}

// LoadDirective parses function comment directive and
// updates function info correspondingly.
func (fn *Func) LoadDirective(directive string) {
	switch directive[len("//goism:"):] {
	case "subst":
		fn.info |= funcSubst
		fn.info |= funcInlineable // Implicitly implied
	case "noinline":
		fn.info |= funcNoinline
	}
}

// Func represents goism function object.
type Func struct {
	Name     string
	Body     Block
	Params   []string
	Variadic bool

	// Maps param index to interface type.
	// Nil if function have no interface parameters at all.
	InterfaceInputs map[int]types.Type

	Results *types.Tuple

	info funcInfo

	DocString string
}
