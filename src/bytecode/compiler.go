package bytecode

import (
	"bytecode/ir"
	"bytes"
	"fmt"
)

type addrHole struct {
	pos      int
	dstBlock int
}

// Compiler converts IR basic blocks into Emacs bytecode.
// Single compiler is intended to be reused for multiple inputs.
type Compiler struct {
	buf       bytes.Buffer
	blocks    []*BasicBlock
	addrHoles []addrHole
	offsets   []uint16
}

func (cl *Compiler) Compile(blocks []*BasicBlock) []byte {
	cl.init(blocks)

	for i, bb := range blocks {
		cl.offsets[i] = uint16(cl.buf.Len())
		cl.compileBlock(bb)
	}
	cl.fillHoles()

	return cl.buf.Bytes()
}

func (cl *Compiler) init(blocks []*BasicBlock) {
	if cl.offsets == nil {
		cl.offsets = make([]uint16, 0, len(blocks))

		// It is hard to predict amount of holes we will need,
		// but there will be some for sure.
		cl.addrHoles = make([]addrHole, 0, len(blocks))
	} else {
		cl.offsets = cl.offsets[:0]
		cl.addrHoles = cl.addrHoles[:0]
	}

	cl.blocks = blocks
}

func (cl *Compiler) compileBlock(bb *BasicBlock) {
	for _, instr := range bb.Instrs {
		switch instr.Op {
		case ir.OpReturn:
			cl.writeOp(125)
		case ir.OpCall:
			cl.writeSpecOp(32, instr.Arg)
		case ir.OpConstRef:
			cl.writeConstRef(instr.Arg)
		case ir.OpStackRef:
			cl.writeStackRef(instr.Arg)
		case ir.OpStackSet:
			cl.writeStackSet(instr.Arg)
		case ir.OpDrop:
			cl.writeDrop(instr.Arg)
		case ir.OpVarRef:
			cl.writeSpecOp(8, instr.Arg)
		case ir.OpVarSet:
			cl.writeSpecOp(16, instr.Arg)
		case ir.OpSetCar:
			cl.writeOp(160)
		case ir.OpSetCdr:
			cl.writeOp(161)
		case ir.OpArrayRef:
			cl.writeOp(72)
		case ir.OpArraySet:
			cl.writeOp(73)
		case ir.OpSubstr:
			cl.writeOp(79)
		case ir.OpConcatN:
			cl.writeConcat(instr.Arg)
		case ir.OpStringEq:
			cl.writeOp(152)
		case ir.OpStringLt:
			cl.writeOp(153)
		case ir.OpToLower:
			cl.writeOp(152)
		case ir.OpToUpper:
			cl.writeOp(151)
		case ir.OpIsCons:
			cl.writeOp(58)
		case ir.OpIsString:
			cl.writeOp(59)
		case ir.OpIsNum:
			cl.writeOp(167)
		case ir.OpIsInt:
			cl.writeOp(168)
		case ir.OpNumAdd:
			cl.writeOp(92)
		case ir.OpNumAdd1:
			cl.writeOp(84)
		case ir.OpNumSub:
			cl.writeOp(90)
		case ir.OpNumSub1:
			cl.writeOp(83)
		case ir.OpNumMul:
			cl.writeOp(95)
		case ir.OpNumDiv:
			cl.writeOp(165)
		case ir.OpNumEq:
			cl.writeOp(85)
		case ir.OpNumLt:
			cl.writeOp(87)
		case ir.OpNumLte:
			cl.writeOp(88)
		case ir.OpNumGt:
			cl.writeOp(86)
		case ir.OpNumGte:
			cl.writeOp(89)
		case ir.OpNumNeg:
			cl.writeOp(91)
		case ir.OpNumMax:
			cl.writeOp(93)
		case ir.OpNumMin:
			cl.writeOp(94)
		case ir.OpRem:
			cl.writeOp(166)
		case ir.OpEq:
			cl.writeOp(61)
		case ir.OpEqual:
			cl.writeOp(154)
		case ir.OpNot:
			cl.writeOp(63)
		case ir.OpMakeList:
			cl.writeMakeList(instr.Arg)
		case ir.OpMakeCons:
			cl.writeOp(66)
		case ir.OpJmp:
			cl.writeJmp(130, instr.Arg)
		case ir.OpJmpNil:
			cl.writeJmp(131, instr.Arg)
		case ir.OpJmpNotNil:
			cl.writeJmp(132, instr.Arg)
		case ir.OpJmpNilElsePop:
			cl.writeJmp(133, instr.Arg)
		case ir.OpJmpNotNilElsePop:
			cl.writeJmp(134, instr.Arg)

		// #FIXME: implement rel jumps.

		case ir.OpCatch:
			cl.writeOp(141)

		default:
			panic(fmt.Sprintf("unexpected instr: %#v\n", instr))
		}
	}
}

func (cl *Compiler) fillHoles() {
	buf := cl.buf.Bytes()

	for _, hole := range cl.addrHoles {
		low, high := imm16Bytes(cl.offsets[hole.dstBlock])
		buf[hole.pos] = low
		buf[hole.pos+1] = high
	}
}

func (cl *Compiler) writeOp(opcode byte) {
	cl.buf.WriteByte(opcode)
}

func (cl *Compiler) writeOp1(opcode byte, arg uint16) {
	cl.buf.Write([]byte{opcode, byte(arg)})
}

func (cl *Compiler) writeOp2(opcode byte, arg uint16) {
	low, high := imm16Bytes(arg)
	cl.buf.Write([]byte{opcode, low, high})
}

func (cl *Compiler) writeJmp(opcode byte, arg uint16) {
	cl.addrHoles = append(cl.addrHoles, addrHole{
		pos:      cl.buf.Len(),
		dstBlock: int(arg),
	})
	cl.buf.Write([]byte{opcode, 0, 0})
}

func (cl *Compiler) writeConstRef(arg uint16) {
	const (
		const1 = 192
		const2 = 129
	)

	if arg <= 0xFF-const1 {
		cl.writeOp(byte(const1 + arg))
	} else {
		cl.writeOp2(const2, arg)
	}
}

func (cl *Compiler) writeStackRef(arg uint16) {
	if arg == 0 {
		// Dup instruction (stackref[0] is invalid).
		cl.writeOp(137)
	} else {
		cl.writeSpecOp(0, arg)
	}
}

func (cl *Compiler) writeStackSet(arg uint16) {
	if arg <= 0xFF {
		cl.writeOp1(178, arg)
	} else {
		cl.writeOp2(179, arg)
	}
}

func (cl *Compiler) writeDrop(arg uint16) {
	if arg == 1 {
		cl.writeOp(136) // discard
	} else {
		cl.writeOp1(182, arg) // discardN
	}
}

func (cl *Compiler) writeMakeList(arg uint16) {
	switch arg {
	case 1, 2, 3, 4:
		cl.writeOp(67 + byte(arg))
	default:
		cl.writeOp1(175, arg)
	}
}

func (cl *Compiler) writeConcat(arg uint16) {
	switch arg {
	case 2, 3, 4:
		cl.writeOp(80 + byte(arg))
	default:
		cl.writeOp1(176, arg)
	}
}

// Write a specialized opcode.
// Specialized opcode has argument 3 encodings
// that depend on the argument value.
func (cl *Compiler) writeSpecOp(opcode byte, arg uint16) {
	// Possible encodings:
	const (
		enc0 = 5      // Arg encoded inside opcode
		enc1 = 0xFF   // Arg encoded as imm8
		enc2 = 0xFFFF // Arg encoded as imm16
	)

	if arg <= enc0 {
		cl.writeOp(opcode + byte(arg))
	} else if arg <= enc1 {
		cl.writeOp1(opcode+6, arg)
	} else {
		cl.writeOp2(opcode+7, arg)
	}
}

func imm16Bytes(x uint16) (byte, byte) {
	low := x & 0x00FF
	high := (x & 0xFF00) >> 8
	return byte(low), byte(high)
}
