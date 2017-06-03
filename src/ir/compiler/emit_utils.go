package compiler

import (
	"ir/instr"
)

func emitN(cl *Compiler, ins instr.Instr, n int) {
	for i := 0; i < n; i++ {
		emit(cl, ins)
	}
}
