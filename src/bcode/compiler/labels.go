package compiler

type labelID int

type label struct {
	addr  int
	jumps []int
}

func labelCreate(cl *Compiler) labelID {
	cl.labels = append(cl.labels, label{})
	return labelID(len(cl.labels) - 1)
}

func labelBind(cl *Compiler, id labelID) {
	cl.labels[id].addr = cl.buf.Len()
}

func labelsResolve(cl *Compiler) {
	code := cl.buf.Bytes()
	for _, l := range cl.labels {
		low, high := imm16Bytes(uint16(l.addr))
		for _, jump := range l.jumps {
			code[jump+1] = low
			code[jump+2] = high
		}
	}
}
