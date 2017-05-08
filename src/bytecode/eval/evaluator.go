package eval

import (
	"bytecode"
	"bytecode/ir"
)

type evaluator struct {
	trace evaluatorTracer
	*bytecode.Object
	stack stack
	argc  int
}

func (ev *evaluator) eval() {
	ev.trace.PreEval(ev)
	for index, bb := range ev.Blocks {
		ev.trace.PreBlock(index, bb.Name)
		ev.evalBlock(bb)
	}
	ev.trace.PostEval(ev)

	ev.StackUsage = ev.stack.maxLen
}

func (ev *evaluator) evalBlock(bb *bytecode.BasicBlock) {
	for i, instr := range bb.Instrs {
		switch instr.Op {
		case ir.OpPanic:
			ev.stack.Drop(2)
			bb.Instrs[i] = ir.Call(1)

		case ir.OpIsCons, ir.OpIsInt, ir.OpIsNum, ir.OpIsString:
			ev.stack.Drop(1)
			ev.stack.PushUnknown()

		case ir.OpConcat:
			ev.stack.Drop(int(instr.Data))
			ev.stack.PushUnknown()

		case ir.OpCall:
			ev.stack.Drop(int(instr.Data) + 1)
			ev.stack.PushUnknown()
		case ir.OpNoreturnCall:
			ev.stack.Drop(int(instr.Data) + 1)
			bb.Instrs[i] = ir.Call(int(instr.Data))

		case ir.OpConstRef:
			ev.stack.Push(ev.ConstPool.Get(instr.Data))

		case ir.OpStackRef:
			ev.stack.Copy(ev.stack.Len() - int(instr.Data) - 1)

		case ir.OpLocalRef:
			index := ev.stack.FindLocal(instr.Data)
			bb.Instrs[i] = ir.StackRef(ev.stack.Len() - index - 1)
			ev.stack.Copy(index)

		case ir.OpLocalSet:
			index := ev.stack.FindLocal(instr.Data)
			bb.Instrs[i] = ir.StackSet(ev.stack.Len() - index - 1)
			ev.stack.items[index].val = ev.stack.Pop().val

		case ir.OpLocalBind:
			ev.stack.BindLocal(instr.Data)
			bb.Instrs[i] = ir.Empty

		case ir.OpScopeExit:
			ev.stack.Drop(int(instr.Data))
			bb.Instrs[i] = ir.Empty

		case ir.OpReturn, ir.OpJmpNil, ir.OpJmpNotNil:
			ev.stack.Drop(1)

		case ir.OpNumAdd, ir.OpNumSub, ir.OpNumMul, ir.OpNumQuo:
			fallthrough
		case ir.OpNumGt, ir.OpNumLt, ir.OpNumEq:
			ev.stack.Drop(2)
			ev.stack.PushUnknown()
		}

		ev.trace.PostInstr(ev, instr)
	}
}
