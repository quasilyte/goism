package instr

func binOp(name string) Instr {
	return Instr{
		Name:     []byte(name),
		Encoding: AttrEnc0,
		Input:    AttrTake2,
		Output:   AttrPushTmp,
	}
}

func unaryOp(name string) Instr {
	return Instr{
		Name:     []byte(name),
		Encoding: AttrEnc0,
		Input:    AttrTake1,
		Output:   AttrPushTmp,
	}
}
