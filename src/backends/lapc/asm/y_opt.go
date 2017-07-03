package asm

import (
	"backends/lapc/ir"
	"vmm"
)

func optimizeY(instrs yUnit) {
	opt := optimizerY{
		instrs: instrs,
	}
	opt.Optimize()
}

type optimizerY struct {
	instrs yUnit
}

func (opt *optimizerY) Optimize() {
	for i, ins := range opt.instrs {
		switch ins.Kind {
		case ir.Call:
			opt.optimizeCall(i, ins)
		case ir.Discard:
			opt.optimizeDiscard(i, ins)
		}
	}
}

func (opt *optimizerY) optimizeCall(pos int, ins ir.Instr) {
	// If function is "throw", remove next instruction if it is Discard.
	if vmm.FuncIsThrowing(ins.Meta) {
		next := opt.instrAt(pos + 1)
		if next.Kind == ir.Discard && next.Data == 1 {
			opt.remove(pos + 1)
		}
	}
}

func (opt *optimizerY) optimizeDiscard(pos int, ins ir.Instr) {
	// Remove Discard(0).
	if ins.Data == 0 {
		opt.remove(pos)
	}
}

func (opt *optimizerY) remove(pos int) {
	opt.instrs[pos] = ir.Instr{Kind: ir.Empty}
}

func (opt *optimizerY) instrAt(pos int) ir.Instr {
	if pos >= len(opt.instrs) {
		return ir.Instr{Kind: ir.Empty}
	}
	if pos < 0 {
		return ir.Instr{Kind: ir.Empty}
	}
	return opt.instrs[pos]
}
