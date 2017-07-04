package lapc

import (
	"backends/lapc/ir"
	"go/types"
	"sexp"
	"vmm"
	"xtypes"
)

// InstrCall is optimized version of Call/Op.
// Used when there is dedicated opcode available.
type InstrCall struct {
	Instr ir.Instr
	Args  []sexp.Form
}

func (call *InstrCall) Copy() sexp.Form {
	return &InstrCall{
		Instr: call.Instr,
		Args:  sexp.CopyList(call.Args),
	}
}

func (call *InstrCall) Cost() int {
	instrName := string(ir.EncodingOf(call.Instr.Kind).Name)
	return sexp.CostOfList(call.Args) + vmm.InstrCallCost(instrName)
}

func (call *InstrCall) Type() types.Type { return xtypes.TypVoid }
