package eval

import (
	"bytecode/ir"
	"cfg"
	"fmt"
	"lisp"
)

type evaluatorTracer struct{}

func (tracer evaluatorTracer) PreEval(ev *evaluator) {
	if cfg.TraceEvaluator {
		print("  pre eval: ")
		tracer.printStack(ev)
	}
}

func (tracer evaluatorTracer) PostEval(ev *evaluator) {
	if cfg.TraceEvaluator {
		print("  post eval: ")
		tracer.printStack(ev)
	}
}

func (evaluatorTracer) PreBlock(index int, name string) {
	if cfg.TraceEvaluator {
		fmt.Printf("%d: # %s\n", index, name)
	}
}

func (tracer evaluatorTracer) PostInstr(ev *evaluator, instr ir.Instr) {
	if cfg.TraceEvaluator {
		fmt.Printf("%22s(%d) ", instr.Op, instr.Data)
		tracer.printStack(ev)
	}
}

func (evaluatorTracer) printStack(ev *evaluator) {
	print("[")
	for _, item := range ev.stack.items {
		if item.localID != nilID {
			fmt.Printf("%s=", ev.Locals[item.localID])
		}

		switch val := item.val.(type) {
		case int64:
			fmt.Printf("%d ", val)
		case float64:
			fmt.Printf("%f ", val)
		case string:
			fmt.Printf("%q ", val)
		case lisp.Symbol:
			fmt.Printf("%s ", string(val))
		case unknownValue:
			print("? ")
		}
	}
	println("]")
}
