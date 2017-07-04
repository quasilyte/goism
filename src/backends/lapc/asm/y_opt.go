package asm

import (
	"backends/lapc/ir"
	"vmm"
)

func optimizeY(u yUnit) {
	opt := optimizerY{}
	opt.Optimize(u)
}

type optimizerY struct{}

func (opt *optimizerY) Optimize(u yUnit) {
	for ins := u; ins != nil; ins = ins.Next {
		switch ins.Kind {
		case ir.Call:
			opt.optimizeCall(ins)
		case ir.Discard:
			opt.optimizeDiscard(ins)
		}
	}
}

func (opt *optimizerY) optimizeCall(ins *ir.Instr) {
	// If function is "throw", remove next instruction if it is Discard(1).
	if vmm.FuncIsThrowing(ins.Meta) {
		if ins.Next.Kind == ir.Discard && ins.Next.Data == 1 {
			ins.Next.Remove()
		}
	}
}

func (opt *optimizerY) optimizeDiscard(ins *ir.Instr) {
	// Remove Discard(0).
	if ins.Data == 0 {
		ins.Remove()
	}
}
