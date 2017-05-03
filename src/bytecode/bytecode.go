package bytecode

// Object is a compiled bytecode unit.
type Object struct {
	Code       []*BasicBlock
	ConstVec   ConstVec
	StackUsage int
}

// Func is a compiled bytecode function.
type Func struct {
	Object

	Arity    int
	Variadic bool

	DocString string
}
