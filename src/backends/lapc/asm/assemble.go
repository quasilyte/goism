package asm

import (
	"backends/lapc/ir"
)

func emit(as *Assembler, ins ir.Instr) {
	enc := ir.EncodingOf(ins.Kind)

	if dst := &as.buf; enc.HasArg {
		writeOp1(dst, enc.Name, ins.Data)
	} else {
		writeOp0(dst, enc.Name)
	}
}

func assembleList(as *Assembler, instrs yUnit) {
	for _, ins := range instrs {
		assembleInstr(as, ins)
	}
}

func assembleInstr(as *Assembler, ins ir.Instr) {
	switch ins.Kind {
	case ir.Empty:
		// Do nothing
	case ir.XvarRef:
		assembleXvarRef(as, ins)
	case ir.XvarSet:
		assembleXvarSet(as, ins)
	case ir.Label:
		assembleLabel(as, ins)
	case ir.Xgoto:
		assembleXgoto(as, ins)
	case ir.Jmp, ir.JmpNil, ir.JmpNotNil, ir.JmpNilElsePop, ir.JmpNotNilElsePop:
		assembleJmp(as, ins)

	default:
		emit(as, ins)
	}
}

func assembleXvarRef(as *Assembler, ins ir.Instr) {
	cvIndex := as.cvec.InsertSym(ins.Meta)
	emit(as, ir.Instr{Kind: ir.VarRef, Data: int32(cvIndex)})
}

func assembleXvarSet(as *Assembler, ins ir.Instr) {
	cvIndex := as.cvec.InsertSym(ins.Meta)
	emit(as, ir.Instr{Kind: ir.VarSet, Data: int32(cvIndex)})
}

func assembleLabel(as *Assembler, ins ir.Instr) {
	writeLabel(&as.buf, ins)
}

func assembleXgoto(as *Assembler, ins ir.Instr) {
	writeLabel(&as.buf, ins)
}

func assembleYdiscard(as *Assembler, ins ir.Instr) {
	writeOp1(&as.buf, ir.EncodingOf(ir.Discard).Name, ins.Data)
}

func assembleJmp(as *Assembler, ins ir.Instr) {
	writeLabel(&as.buf, ins)
}
