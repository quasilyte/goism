package asm

import (
	"backends/lapc/ir"
	"bytes"
	"dt"
)

type Assembler struct {
	buf  bytes.Buffer
	unit *ir.Unit
	cvec *dt.ConstPool
}

type Object struct {
	Code       []byte
	StackUsage int
}

func NewAssembler(cvec *dt.ConstPool) *Assembler {
	return &Assembler{
		cvec: cvec,
	}
}

func (as *Assembler) Assemble(params []string, u *ir.Unit) Object {
	as.reset(params, u)

	xUnit := xUnit(u.Instrs)
	optimizeX(xUnit)

	yUnit, maxStackLen := makeY(params, u.Instrs)
	optimizeY(yUnit)

	assembleList(as, yUnit)

	return Object{
		Code:       as.buf.Bytes(),
		StackUsage: maxStackLen,
	}
}

func (as *Assembler) reset(params []string, u *ir.Unit) {
	as.unit = u
	as.buf.Truncate(0)
}
