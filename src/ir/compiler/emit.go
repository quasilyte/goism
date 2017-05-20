package compiler

import (
	"fmt"
	"ir"
)

func emit(cl *Compiler, instr ir.Instr) {
	// Do not split this function into parts
	// to avoid unnecessary emitX functions.

	// General scheme:
	// 1) consume stack arguments.
	// 2) write instruction to dst.
	// 3) push results to stack (if any).

	switch dst := &cl.buf; instr.Kind {
	case ir.InstrBinOp, ir.InstrCellSet:
		cl.st.Discard(2)
		writeOp0(dst, instr)
		cl.st.Push()

	case ir.InstrUnaryOp, ir.InstrCellRef:
		cl.st.Discard(1)
		writeOp0(dst, instr)
		cl.st.Push()

	case ir.InstrArraySet:
		cl.st.Discard(3)
		writeOp0(dst, instr)
		cl.st.Push()
		emit(cl, ir.Discard(1))

	case ir.InstrConstRef:
		writeOp1(dst, instr)
		cl.st.PushConst(instr.Data)

	case ir.InstrRet:
		cl.st.Discard(instr.Data)
		writeOp0(dst, instr)

	case ir.InstrStackRef:
		writeOp1(dst, instr)
		cl.st.Dup(instr.Data)

	case ir.InstrVarRef:
		writeOp1(dst, instr)
		cl.st.Push()

	case ir.InstrStackSet, ir.InstrVarSet:
		cl.st.Discard(1)
		writeOp1(dst, instr)

	case ir.InstrDiscard:
		cl.st.Discard(instr.Data)
		writeOp1(dst, instr)

	case ir.InstrCall:
		cl.st.Discard(instr.Data + 1)
		writeOp1(dst, instr)
		cl.st.Push()

	case ir.InstrPanicCall:
		cl.st.Discard(instr.Data + 1)
		writeOp1(dst, instr)

	case ir.InstrVoidCall:
		emit(cl, ir.Call(int(instr.Data)))
		emit(cl, ir.Discard(1))

	default:
		panic(fmt.Sprintf("unexpected instr: %#v\n", instr))
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
