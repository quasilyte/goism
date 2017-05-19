package ir

func binOp(name string) Instr {
	return Instr{
		Name: []byte(name),
		Kind: InstrBinOp,
	}
}

func cellSet(name string) Instr {
	return Instr{
		Name: []byte(name),
		Kind: InstrCellSet,
	}
}

func cellRef(name string) Instr {
	return Instr{
		Name: []byte(name),
		Kind: InstrCellRef,
	}
}

func call(argc int, kind InstrKind) Instr {
	return Instr{
		Name: []byte("call"),
		Data: uint16(argc),
		Kind: kind,
	}
}
