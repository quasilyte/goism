package compiler

import (
	"ir/instr"
)

func emit(cl *Compiler, ins instr.Instr) {
	switch ins.Input {
	case instr.AttrTakeNothing:
		/* Do nothing */
	case instr.AttrTake1:
		cl.st.Discard(1)
	case instr.AttrTake2:
		cl.st.Discard(2)
	case instr.AttrTake3:
		cl.st.Discard(3)
	case instr.AttrTakeN:
		cl.st.Discard(ins.Data)
	case instr.AttrTakeNplus1:
		cl.st.Discard(ins.Data + 1)
	case instr.AttrReplaceNth:
		// "-1" because we popped stask element.
		cl.st.Rebind(int(ins.Data)-1, cl.st.Pop())
	}

	switch dst := &cl.buf; ins.Encoding {
	case instr.AttrEnc0:
		writeOp0(dst, ins)
	case instr.AttrEnc1:
		writeOp1(dst, ins)
	}

	switch ins.Output {
	case instr.AttrPushNothing:
		/* Do nothing */
	case instr.AttrDupNth:
		cl.st.Dup(ins.Data)
	case instr.AttrPushTmp:
		cl.st.Push()
	case instr.AttrPushConst:
		cl.st.PushConst(ins.Data)
	case instr.AttrPushAndDiscard:
		cl.st.Push()
		emit(cl, instr.Discard(1))
	}
}

func emitJmp(cl *Compiler, label label) {
	writeLabel(&cl.buf, instr.Jmp, label)
}

func emitJmpNil(cl *Compiler, label label) {
	cl.st.Discard(1)
	writeLabel(&cl.buf, instr.JmpNil, label)
}

func emitJmpNotNil(cl *Compiler, label label) {
	cl.st.Discard(1)
	writeLabel(&cl.buf, instr.JmpNotNil, label)
}
