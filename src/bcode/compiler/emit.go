package compiler

import (
	"bcode"
	"fmt"
)

func emit(cl *Compiler, instr bcode.Instr) {
	// Do not split this function into parts
	// to avoid unnecessary emitX functions.
	//
	// emitJmpX functions are special and they must be
	// distinct from normal emit function (which is
	// impossible when we have dozens of emitters).
	fmt.Printf("%v %v\n", instr, cl.st)
	switch dst := &cl.buf; instr.Kind {
	case bcode.InstrBinOp:
		cl.st.Drop(2)
		writeOp(dst, instr.Opcode)
		cl.st.Push()

	case bcode.InstrRet:
		cl.st.Drop(instr.Data)
		cl.buf.WriteByte(instr.Opcode)

	case bcode.InstrConstRef:
		const (
			constant1 = 192
			constant2 = 129
		)
		if instr.Data <= 0xFF-constant1 {
			writeOp(dst, byte(constant1+instr.Data))
		} else {
			writeOp2(dst, constant2, instr.Data)
		}
		cl.st.PushConst(instr.Data)

	case bcode.InstrStackRef:
		if instr.Data == 0 {
			writeOp(dst, 137) // "dup".
		} else {
			writeTriOp(dst, 0, instr.Data)
		}
		cl.st.Push()

	case bcode.InstrStackSet:
		cl.st.Drop(1)
		if instr.Data <= 0xFF {
			writeOp1(dst, 178, instr.Data)
		} else {
			writeOp2(dst, 179, instr.Data)
		}

	case bcode.InstrDrop:
		cl.st.Drop(instr.Data)
		if instr.Data == 1 {
			writeOp(dst, 136) // "discard".
		} else {
			writeOp1(dst, 182, instr.Data) // "discardN".
		}

	case bcode.InstrCall:
		cl.st.Drop(instr.Data + 1)
		writeTriOp(dst, 32, instr.Data)
		cl.st.Push()

	case bcode.InstrScopeExitingCall:
		cl.st.Drop(instr.Data + 1)
		writeTriOp(dst, 32, instr.Data)

	case bcode.InstrVoidCall:
		emit(cl, bcode.Call(int(instr.Data)))
		emit(cl, bcode.Drop(1))

	default:
		panic(fmt.Sprintf("unexpected instr: %#v\n", instr))
	}
}

func emitJmp(cl *Compiler, id labelID) {
	cl.labels[id].jumps = append(cl.labels[id].jumps, cl.buf.Len())
	writeOp2(&cl.buf, 130, 0)
}

func emitJmpNil(cl *Compiler, id labelID) {
	cl.labels[id].jumps = append(cl.labels[id].jumps, cl.buf.Len())
	cl.st.Drop(1)
	writeOp2(&cl.buf, 131, 0)
}
