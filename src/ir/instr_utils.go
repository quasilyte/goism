package ir

func binOp(name string) Instr {
	return Instr{
		Name:     []byte(name),
		Encoding: InstrEnc0,
		Input:    InstrTake2,
		Output:   InstrPushTmp,
	}
}

func unaryOp(name string) Instr {
	return Instr{
		Name:     []byte(name),
		Encoding: InstrEnc0,
		Input:    InstrTake1,
		Output:   InstrPushTmp,
	}
}
