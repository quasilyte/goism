package ir

// Jump instruction names.
var (
	Jmp       = []byte("goto")
	JmpNil    = []byte("goto-if-nil")
	JmpNotNil = []byte("goto-if-not-nil")
	Label     = []byte("label")
)

// Binary ops.
var (
	NumEq    = binOp("num=")
	NumGt    = binOp("num>")
	NumLt    = binOp("num<")
	NumSub   = binOp("sub")
	NumAdd   = binOp("add")
	NumMul   = binOp("mul")
	NumQuo   = binOp("quo")
	ArrayRef = binOp("array-ref")

	SetCar = binOp("setcar")
	SetCdr = binOp("setcdr")
)

// Unary ops.
var (
	Add1 = unaryOp("add1")
	Sub1 = unaryOp("sub1")

	Car = unaryOp("car")
	Cdr = unaryOp("cdr")
)

// Other ops without explicit parameter.
var (
	ArraySet = Instr{
		Name:     []byte("array-set"),
		Encoding: InstrEnc0,
		Input:    InstrTake3,
		Output:   InstrPushAndDiscard,
	}

	Return = Instr{
		Name:     []byte("return"),
		Encoding: InstrEnc0,
		Input:    InstrTake1,
	}
)

func Concat(argc int) Instr {
	return Instr{
		Name:     []byte("concat"),
		Encoding: InstrEnc1,
		Input:    InstrTakeN,
		Output:   InstrPushTmp,
		Data:     uint16(argc),
	}
}

func StackSet(stIndex int) Instr {
	return Instr{
		Name:     []byte("stack-set"),
		Encoding: InstrEnc1,
		Input:    InstrTake1,
		Data:     uint16(stIndex),
	}
}

func VarSet(cvIndex int) Instr {
	return Instr{
		Name:     []byte("var-set"),
		Encoding: InstrEnc1,
		Input:    InstrTake1,
		Data:     uint16(cvIndex),
	}
}

func Discard(n int) Instr {
	return Instr{
		Name:     []byte("discard"),
		Encoding: InstrEnc1,
		Input:    InstrTakeN,
		Data:     uint16(n),
	}
}

func ConstRef(cvIndex int) Instr {
	return Instr{
		Name:     []byte("constant"),
		Encoding: InstrEnc1,
		Output:   InstrPushConst,
		Data:     uint16(cvIndex),
	}
}

func StackRef(stIndex int) Instr {
	return Instr{
		Name:     []byte("stack-ref"),
		Encoding: InstrEnc1,
		Output:   InstrDupNth,
		Data:     uint16(stIndex),
	}
}

func VarRef(cvIndex int) Instr {
	return Instr{
		Name:     []byte("var-ref"),
		Encoding: InstrEnc1,
		Output:   InstrPushTmp,
		Data:     uint16(cvIndex),
	}
}

func Call(argc int) Instr {
	return Instr{
		Name:     []byte("call"),
		Encoding: InstrEnc1,
		Input:    InstrTakeNplus1,
		Output:   InstrPushTmp,
		Data:     uint16(argc),
	}
}
