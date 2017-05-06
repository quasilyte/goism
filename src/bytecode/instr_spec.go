package bytecode

import (
	"bytecode/ir"
	"emacs/lisp"
)

type outputMode int

const (
	// Instruction outputs 1 value.
	omTmp outputMode = iota
	// Instruction outputs 1 value and it must be discarded immediately.
	omDiscard
	// Instruction does not produce any output.
	omVoid
)

type instrSpec struct {
	argc int
	mode outputMode
	fn   lisp.Symbol
}

var instrSpecs = [...]instrSpec{
	ir.OpStackSet: {argc: 1, mode: omVoid},
	ir.OpReturn:   {argc: 1, mode: omVoid},
	ir.OpDrop:     {argc: 0, mode: omVoid},

	ir.OpNumAdd: {argc: 2, mode: omTmp, fn: "+"},
	ir.OpNumSub: {argc: 2, mode: omTmp, fn: "-"},
	ir.OpNumMul: {argc: 2, mode: omTmp, fn: "*"},
	ir.OpNumQuo: {argc: 2, mode: omTmp, fn: "/"},
	ir.OpNumGt:  {argc: 2, mode: omTmp, fn: ">"},
	ir.OpNumLt:  {argc: 2, mode: omTmp, fn: ">"},
	ir.OpNumEq:  {argc: 2, mode: omTmp, fn: "="},

	ir.OpMakeCons: {argc: 2, mode: omTmp},

	ir.OpCar: {argc: 1, mode: omTmp},
	ir.OpCdr: {argc: 1, mode: omTmp},
}
