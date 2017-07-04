package compiler

import (
	"backends/lapc"
	"backends/lapc/asm"
	"backends/lapc/ir"
	"dt"
	"sexp"
)

type Compiler struct {
	cvec *dt.ConstPool

	unit *ir.Unit
	as   *asm.Assembler

	innerBreak     ir.Instr // Innermost "break" target label
	innerContinue  ir.Instr // Innermost "continue" target label
	innerInlineRet ir.Instr // Innermost IIFE "return" target label
}

func New() *Compiler {
	cvec := &dt.ConstPool{}
	return &Compiler{
		cvec: cvec,
		unit: ir.NewUnit(),
		as:   asm.NewAssembler(cvec),
	}
}

func (cl *Compiler) CompileFunc(fn *sexp.Func) *lapc.Object {
	cl.reset()

	compileStmtList(cl, fn.Body.Forms)
	cl.push().Empty() // Add sentinel ir.Empty instruction

	asmObject := cl.as.Assemble(fn.Params, cl.unit)

	return &lapc.Object{
		StackUsage: asmObject.StackUsage,
		Code:       asmObject.Code,
		ConstVec:   cl.cvec,
	}
}

// Prepare compiler for re-use.
func (cl *Compiler) reset() {
	cl.cvec.Clear()
	cl.unit.Init()
}

func (cl *Compiler) push() *ir.InstrPusher {
	return cl.unit.InstrPusher()
}

func (cl *Compiler) pushN(ins ir.Instr, n int) {
	for i := 0; i < n; i++ {
		cl.pushInstr(ins)
	}
}

func (cl *Compiler) pushInstr(ins ir.Instr) {
	cl.push().PushInstr(ins)
}
