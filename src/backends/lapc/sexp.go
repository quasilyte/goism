package lapc

import (
	"backends/lapc/instr"
	"go/types"
	"sexp"
	"vmm"
	"xtypes"
)

// InstrCall is optimized version of Call/Op.
// Used when there is dedicated opcode available.
type InstrCall struct {
	Instr instr.Instr
	Args  []sexp.Form
}

func (call *InstrCall) Copy() sexp.Form {
	return &InstrCall{
		Instr: call.Instr,
		Args:  sexp.CopyList(call.Args),
	}
}

func (call *InstrCall) Cost() int {
	instrName := string(call.Instr.Name)
	return sexp.CostOfList(call.Args) + vmm.InstrCallCost(instrName)
}

func (call *InstrCall) Type() types.Type { return xtypes.TypVoid }
