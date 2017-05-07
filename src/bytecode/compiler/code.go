package compiler

import (
	"bytecode"
	"bytecode/ir"
)

type code struct {
	blocks  []*bytecode.BasicBlock
	current *bytecode.BasicBlock
}

func newCode() *code {
	bb := &bytecode.BasicBlock{Name: "entry"}
	code := &code{
		blocks:  []*bytecode.BasicBlock{bb},
		current: bb,
	}
	return code
}

func (c *code) pushBlock(name string) {
	c.current = &bytecode.BasicBlock{Name: name}
	c.blocks = append(c.blocks, c.current)
}

func (c *code) block() *bytecode.BasicBlock {
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

func (c *code) lastInstr() ir.Instr {
	for i := len(c.blocks) - 1; i >= 0; i-- {
		bb := c.blocks[i]
		if len(bb.Instrs) == 0 {
			continue // Empty block
		}
		return bb.Instrs[len(bb.Instrs)-1]
	}
	return ir.Empty
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
