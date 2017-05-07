package bytecode

import (
	"bytecode/ir"
	"bytes"
	"fmt"
)

type addrHole struct {
	// Position (offset) inside bytecode where imm16
	// address is missing.
	pos int
	// Basic block index; its offset is jump target.
	dstBlock int
}

// Assembler converts IR basic blocks into Emacs bytecode.
// Single assembler is intended to be reused for multiple inputs.
type Assembler struct {
	buf       bytes.Buffer
	blocks    []*BasicBlock
	addrHoles []addrHole
	offsets   []uint16
}

// Assemble creates executable Emacs Lisp bytecode.
func (asm *Assembler) Assemble(blocks []*BasicBlock) []byte {
	asm.init(blocks)

	for _, bb := range blocks {
		asm.offsets = append(asm.offsets, uint16(asm.buf.Len()))
		asm.assembleBlock(bb)
	}
	asm.fillHoles()

	return asm.buf.Bytes()
}

func (asm *Assembler) init(blocks []*BasicBlock) {
	if asm.offsets == nil {
		asm.offsets = make([]uint16, 0, len(blocks))

		// It is hard to predict amount of holes we will need,
		// but there will be some for sure.
		asm.addrHoles = make([]addrHole, 0, len(blocks))
	} else {
		asm.offsets = asm.offsets[:0]
		asm.addrHoles = asm.addrHoles[:0]
	}

	asm.blocks = blocks
}

func (asm *Assembler) assembleBlock(bb *BasicBlock) {
	for _, instr := range bb.Instrs {
		asm.assembleInstr(instr)
	}
}

func (asm *Assembler) assembleInstr(instr ir.Instr) {
	switch instr.Op {
	case ir.OpReturn:
		asm.writeOp(135)
	case ir.OpCall:
		asm.writeSpecOp(32, instr.Data)
	case ir.OpConstRef:
		asm.writeConstRef(instr.Data)
	case ir.OpStackRef:
		asm.writeStackRef(instr.Data)
	case ir.OpStackSet:
		asm.writeStackSet(instr.Data)
	case ir.OpDrop:
		asm.writeDrop(instr.Data)
	case ir.OpVarRef:
		asm.writeSpecOp(8, instr.Data)
	case ir.OpVarSet:
		asm.writeSpecOp(16, instr.Data)
	case ir.OpSetCar:
		asm.writeOp(160)
	case ir.OpSetCdr:
		asm.writeOp(161)
	case ir.OpCar:
		asm.writeOp(64)
	case ir.OpCdr:
		asm.writeOp(65)

	case ir.OpArrayRef:
		asm.writeOp(72)
	case ir.OpArraySet:
		asm.writeOp(73)
	case ir.OpSubstr:
		asm.writeOp(79)
	case ir.OpConcat:
		asm.writeConcat(instr.Data)
	case ir.OpStringEq:
		asm.writeOp(152)
	case ir.OpStringLt:
		asm.writeOp(153)
	case ir.OpToLower:
		asm.writeOp(152)
	case ir.OpToUpper:
		asm.writeOp(151)
	case ir.OpIsCons:
		asm.writeOp(58)
	case ir.OpIsString:
		asm.writeOp(59)
	case ir.OpIsNum:
		asm.writeOp(167)
	case ir.OpIsInt:
		asm.writeOp(168)
	case ir.OpNumAdd:
		asm.writeOp(92)
	case ir.OpNumAdd1:
		asm.writeOp(84)
	case ir.OpNumSub:
		asm.writeOp(90)
	case ir.OpNumSub1:
		asm.writeOp(83)
	case ir.OpNumMul:
		asm.writeOp(95)
	case ir.OpNumQuo:
		asm.writeOp(165)
	case ir.OpNumEq:
		asm.writeOp(85)
	case ir.OpNumLt:
		asm.writeOp(87)
	case ir.OpNumLte:
		asm.writeOp(88)
	case ir.OpNumGt:
		asm.writeOp(86)
	case ir.OpNumGte:
		asm.writeOp(89)
	case ir.OpNumNeg:
		asm.writeOp(91)
	case ir.OpNumMax:
		asm.writeOp(93)
	case ir.OpNumMin:
		asm.writeOp(94)
	case ir.OpRem:
		asm.writeOp(166)
	case ir.OpEq:
		asm.writeOp(61)
	case ir.OpEqual:
		asm.writeOp(154)
	case ir.OpNot:
		asm.writeOp(63)
	case ir.OpMakeList:
		asm.writeMakeList(instr.Data)
	case ir.OpMakeCons:
		asm.writeOp(66)
	case ir.OpJmp:
		asm.writeJmp(130, instr.Data)
	case ir.OpJmpNil:
		asm.writeJmp(131, instr.Data)
	case ir.OpJmpNotNil:
		asm.writeJmp(132, instr.Data)
	case ir.OpJmpNilElsePop:
		asm.writeJmp(133, instr.Data)
	case ir.OpJmpNotNilElsePop:
		asm.writeJmp(134, instr.Data)
	case ir.OpRelJmp:
		asm.writeOp1(170, instr.Data)
	case ir.OpRelJmpNil:
		asm.writeOp1(171, instr.Data)
	case ir.OpRelJmpNotNil:
		asm.writeOp1(172, instr.Data)
	case ir.OpRelJmpNilElsePop:
		asm.writeOp1(173, instr.Data)
	case ir.OpRelJmpNotNilElsePop:
		asm.writeOp1(174, instr.Data)
	case ir.OpCatch:
		asm.writeOp(141)

	default:
		panic(fmt.Sprintf("unexpected instr: %#v\n", instr))
	}
}

// Resolve all missing jump targets.
func (asm *Assembler) fillHoles() {
	buf := asm.buf.Bytes()

	for _, hole := range asm.addrHoles {
		low, high := imm16Bytes(asm.offsets[hole.dstBlock])
		buf[hole.pos+1] = low
		buf[hole.pos+2] = high
	}
}

func (asm *Assembler) writeOp(opcode byte) {
	asm.buf.WriteByte(opcode)
}

func (asm *Assembler) writeOp1(opcode byte, arg uint16) {
	asm.buf.Write([]byte{opcode, byte(arg)})
}

func (asm *Assembler) writeOp2(opcode byte, arg uint16) {
	low, high := imm16Bytes(arg)
	asm.buf.Write([]byte{opcode, low, high})
}

func (asm *Assembler) writeJmp(opcode byte, arg uint16) {
	// Because we do linear processing, we can not
	// resolve absolute forward jumps.
	// For each such jump we save addrHole to fill
	// address later on, when all offsets will be available.
	//
	// Note that is is possible to resolve backwards jumps here.
	asm.addrHoles = append(asm.addrHoles, addrHole{
		pos:      asm.buf.Len(),
		dstBlock: int(arg),
	})
	asm.buf.Write([]byte{opcode, 0, 0})
}

func (asm *Assembler) writeConstRef(arg uint16) {
	const (
		const1 = 192
		const2 = 129
	)

	if arg <= 0xFF-const1 {
		asm.writeOp(byte(const1 + arg))
	} else {
		asm.writeOp2(const2, arg)
	}
}

func (asm *Assembler) writeStackRef(arg uint16) {
	if arg == 0 {
		// Dup instruction (stackref[0] is invalid).
		asm.writeOp(137)
	} else {
		asm.writeSpecOp(0, arg)
	}
}

func (asm *Assembler) writeStackSet(arg uint16) {
	if arg <= 0xFF {
		asm.writeOp1(178, arg)
	} else {
		asm.writeOp2(179, arg)
	}
}

func (asm *Assembler) writeDrop(arg uint16) {
	if arg == 1 {
		asm.writeOp(136) // discard
	} else {
		asm.writeOp1(182, arg) // discardN
	}
}

func (asm *Assembler) writeMakeList(arg uint16) {
	switch arg {
	case 1, 2, 3, 4:
		asm.writeOp(67 + byte(arg))
	default:
		asm.writeOp1(175, arg) // listN
	}
}

func (asm *Assembler) writeConcat(arg uint16) {
	switch arg {
	case 2, 3, 4:
		asm.writeOp(80 + byte(arg))
	default:
		asm.writeOp1(176, arg) // concatN
	}
}

// Write a specialized opcode.
// Specialized opcode has argument 3 encodings
// that depend on the argument value.
func (asm *Assembler) writeSpecOp(opcode byte, arg uint16) {
	// Possible encodings:
	const (
		enc0 = 5      // Arg encoded inside opcode
		enc1 = 0xFF   // Arg encoded as imm8
		enc2 = 0xFFFF // Arg encoded as imm16
	)

	if arg <= enc0 {
		asm.writeOp(opcode + byte(arg))
	} else if arg <= enc1 {
		asm.writeOp1(opcode+6, arg)
	} else {
		asm.writeOp2(opcode+7, arg)
	}
}

func imm16Bytes(x uint16) (byte, byte) {
	low := x & 0x00FF
	high := (x & 0xFF00) >> 8
	return byte(low), byte(high)
}
