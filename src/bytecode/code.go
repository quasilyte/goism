package bytecode

import "bytecode/ir"

type code struct {
	blocks  []*BasicBlock
	current *BasicBlock
}

func newCode() *code {
	bb := &BasicBlock{Name: "entry"}
	code := &code{
		blocks:  []*BasicBlock{bb},
		current: bb,
	}
	return code
}

func (c *code) pushBlock(name string) {
	c.current = &BasicBlock{Name: name}
	c.blocks = append(c.blocks, c.current)
}

func (c *code) block() *BasicBlock {
	return c.current
}

func (c *code) pushOp(op ir.Opcode) {
	c.current.Instrs = append(c.current.Instrs, ir.Instr{Op: op})
}

func (c *code) pushInstr(instr ir.Instr) {
	c.current.Instrs = append(c.current.Instrs, instr)
}

func (c *code) bindJmp(jr jmpRef) {
	offset := uint16(len(c.blocks) - 1)
	c.blocks[jr.blockIndex].Instrs[jr.instrIndex].Data = offset
}

func (c *code) pushJmp(op ir.Opcode) jmpRef {
	c.pushOp(op)
	return jmpRef{
		blockIndex: len(c.blocks) - 1,
		instrIndex: len(c.current.Instrs) - 1,
		code:       c,
	}
}

type jmpRef struct {
	blockIndex int
	instrIndex int
	code       *code
}

func (jr *jmpRef) bind() {
	offset := uint16(len(jr.code.blocks) - 1)
	jr.code.blocks[jr.blockIndex].Instrs[jr.instrIndex].Data = offset
}
