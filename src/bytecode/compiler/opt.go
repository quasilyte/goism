package compiler

import (
	"bytecode/ir"
	"lisp"
	"sexp"
)

func optCall(cl *Compiler, fn lisp.Symbol, args []sexp.Form) bool {
	switch fn {
	case "cons":
		cl.compileInstr(ir.MakeCons, 2, args)
	case "car":
		cl.compileInstr(ir.Car, 1, args)
	case "cdr":
		cl.compileInstr(ir.Cdr, 1, args)

	default:
		return false
	}
	return true
}

func optAddSub(cl *Compiler, op ir.Opcode, args []sexp.Form) bool {
	numExtract := func(x sexp.Form) int {
		switch x := x.(type) {
		case sexp.Int:
			if x.Val == 1 || x.Val == 2 {
				return int(x.Val)
			}
			return 0
		case sexp.Float:
			if x.Val == 1.0 {
				return 1
			} else if x.Val == 2.0 {
				return 2
			}
			return 0
		default:
			return 0
		}
	}
	specialize := func(num int, arg sexp.Form, instr ir.Instr) bool {
		if num == 1 || num == 2 {
			cl.compileExpr(arg)
			for i := 0; i < num; i++ {
				cl.emit(instr)
			}
			return true
		}
		return false
	}
	specInstr := func(op ir.Opcode) ir.Instr {
		if op == ir.OpNumAdd {
			return ir.NumAdd1
		}
		return ir.NumSub1
	}

	if len(args) == 2 {
		instr := specInstr(op)
		return specialize(numExtract(args[0]), args[1], instr) ||
			specialize(numExtract(args[1]), args[0], instr)
	}
	return false
}
