package compiler

import (
	"elapc/instr"
)

func emitN(cl *Compiler, ins instr.Instr, n int) {
	for i := 0; i < n; i++ {
		emit(cl, ins)
	}
}
