package bcode

import (
	"dt"
)

// Func is a compiled bytecode function.
type Func struct {
	ArgsDesc   uint32
	StackUsage int
	Bytecode   []byte
	ConstVec   *dt.ConstPool
	DocString  string
}
