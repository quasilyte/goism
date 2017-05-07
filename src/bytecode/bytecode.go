package bytecode

import "bytecode/ir"

// BasicBlock is a sequence of instructions
// that has single entry and exit points.
type BasicBlock struct {
	Instrs []ir.Instr

	// Basic block label.
	// Used to simplify debug.
	Name string
}

// Object is a compiled bytecode unit.
type Object struct {
	Blocks     []*BasicBlock
	ConstPool  ConstPool
	StackUsage int      // Peak stack usage.
	Locals     []string // Locals referenced inside object
}

// Func is a compiled bytecode function.
type Func struct {
	Object

	ArgsDesc  uint32
	DocString string
}
