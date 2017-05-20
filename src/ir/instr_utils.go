package ir

func binOp(name string) Instr {
	return Instr{
		Name: []byte(name),
		Kind: InstrBinOp,
	}
}

func variadicOp(argc int, name string) Instr {
	return Instr{
		Name: []byte(name),
		Kind: InstrBinOp,
		Data: uint16(argc),
	}
}

func unaryOp(name string) Instr {
	return Instr{
		Name: []byte(name),
		Kind: InstrUnaryOp,
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
