package compiler

import (
	"ir/instr"
	"xast"
)

// Various data that is used during compilation.

// All operations that can be compiled to IR.
var binOpTable = [...]instr.Instr{
	xast.OpNumAdd:      instr.NumAdd,
	xast.OpNumSub:      instr.NumSub,
	xast.OpNumMul:      instr.NumMul,
	xast.OpNumQuo:      instr.NumQuo,
	xast.OpNumEq:       instr.NumEq,
	xast.OpNumLt:       instr.NumLt,
	xast.OpNumGt:       instr.NumGt,
	xast.OpNumLte:      instr.NumLte,
	xast.OpNumGte:      instr.NumGte,
	xast.OpConcat:      instr.Concat2,
	xast.OpStrEq:       instr.StrEq,
	xast.OpStrLt:       instr.StrLt,
	xast.OpUnsupported: instr.Invalid,
}
