package compiler

import (
	"ir"
)

func emit(cl *Compiler, instr ir.Instr) {
	switch instr.Input {
	case ir.InstrTakeNothing:
		/* Do nothing */
	case ir.InstrTake1:
		cl.st.Discard(1)
	case ir.InstrTake2:
		cl.st.Discard(2)
	case ir.InstrTake3:
		cl.st.Discard(3)
	case ir.InstrTakeN:
		cl.st.Discard(instr.Data)
	case ir.InstrTakeNplus1:
		cl.st.Discard(instr.Data + 1)
	}

	switch dst := &cl.buf; instr.Encoding {
	case ir.InstrEnc0:
		writeOp0(dst, instr)
	case ir.InstrEnc1:
		writeOp1(dst, instr)
	}

	switch instr.Output {
	case ir.InstrPushNothing:
		/* Do nothing */
	case ir.InstrDupNth:
		cl.st.Dup(instr.Data)
	case ir.InstrPushTmp:
		cl.st.Push()
	case ir.InstrPushConst:
		cl.st.PushConst(instr.Data)
	case ir.InstrPushAndDiscard:
		cl.st.Push()
		emit(cl, ir.Discard(1))
	}
}

func emitJmp(cl *Compiler, label label) {
	writeLabel(&cl.buf, ir.Jmp, label)
}

func emitJmpNil(cl *Compiler, label label) {
	cl.st.Discard(1)
	writeLabel(&cl.buf, ir.JmpNil, label)
}

func emitJmpNotNil(cl *Compiler, label label) {
	cl.st.Discard(1)
	writeLabel(&cl.buf, ir.JmpNotNil, label)
}
