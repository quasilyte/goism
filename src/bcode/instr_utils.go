package bcode

func binOp(opcode byte) Instr {
	return Instr{Opcode: opcode, Kind: InstrBinOp}
}
