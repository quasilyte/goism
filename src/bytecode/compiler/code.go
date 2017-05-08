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

func (c *code) bindJmp(jr jmpLabel) {
	offset := uint16(len(c.blocks) - 1)
	c.blocks[jr.blockIndex].Instrs[jr.instrIndex].Data = offset
}

func (c *code) pushJmp(op ir.Opcode) jmpLabel {
	c.pushOp(op)
	return jmpLabel{
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
		for j := len(bb.Instrs) - 1; j >= 0; j-- {
			if bb.Instrs[j] != ir.Empty {
				return bb.Instrs[j]
			}
		}
	}
	return ir.Empty
}

type jmpLabel struct {
	blockIndex int
	instrIndex int
	code       *code
}

func (jr *jmpLabel) bind(labelName string) {
	jr.code.pushBlock(labelName)
	offset := uint16(len(jr.code.blocks) - 1)
	jr.code.blocks[jr.blockIndex].Instrs[jr.instrIndex].Data = offset
}
