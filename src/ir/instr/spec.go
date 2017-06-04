package instr

// Jump instruction names.
var (
	Jmp              = []byte("goto")
	JmpNil           = []byte("goto-if-nil")
	JmpNotNil        = []byte("goto-if-not-nil")
	JmpNilElsePop    = []byte("goto-if-nil-else-pop")
	JmpNotNilElsePop = []byte("goto-if-not-nil-else-pop")
	Label            = []byte("label")
)

// Binary ops.
var (
	NumEq    = binOp("num=")
	NumGt    = binOp("num>")
	NumLt    = binOp("num<")
	NumLte   = binOp("num<=")
	NumGte   = binOp("num>=")
	NumSub   = binOp("sub")
	NumAdd   = binOp("add")
	NumMul   = binOp("mul")
	NumQuo   = binOp("quo")
	StrEq    = binOp("str=")
	StrLt    = binOp("str<")
	ArrayRef = binOp("array-ref")

	Cons   = binOp("cons")
	Memq   = binOp("memq")
	Member = binOp("member")

	Concat2 = Concat(2)
)

// Unary ops.
var (
	Add1 = unaryOp("add1")
	Sub1 = unaryOp("sub1")

	Not = unaryOp("not")
	Neg = unaryOp("neg")

	Car = unaryOp("car")
	Cdr = unaryOp("cdr")

	Length = unaryOp("length")
)

// Other ops without explicit parameter.
var (
	ArraySet = Instr{
		Name:     []byte("array-set"),
		Encoding: AttrEnc0,
		Input:    AttrTake3,
		Output:   AttrPushAndDiscard,
	}

	Return = Instr{
		Name:     []byte("return"),
		Encoding: AttrEnc0,
		Input:    AttrTake1,
	}

	Substr = Instr{
		Name:     []byte("substr"),
		Encoding: AttrEnc0,
		Input:    AttrTake3,
		Output:   AttrPushTmp,
	}
	SetCar = Instr{
		Name:     []byte("setcar"),
		Encoding: AttrEnc0,
		Input:    AttrTake2,
		Output:   AttrPushAndDiscard,
	}
	SetCdr = Instr{
		Name:     []byte("setcdr"),
		Encoding: AttrEnc0,
		Input:    AttrTake2,
		Output:   AttrPushAndDiscard,
	}
)

func Concat(argc int) Instr {
	return Instr{
		Name:     []byte("concat"),
		Encoding: AttrEnc1,
		Input:    AttrTakeN,
		Output:   AttrPushTmp,
		Data:     uint16(argc),
	}
}

func List(argc int) Instr {
	return Instr{
		Name:     []byte("list"),
		Encoding: AttrEnc1,
		Input:    AttrTakeN,
		Output:   AttrPushTmp,
		Data:     uint16(argc),
	}
}

func StackSet(stIndex int) Instr {
	return Instr{
		Name:     []byte("stack-set"),
		Encoding: AttrEnc1,
		Input:    AttrReplaceNth,
		Data:     uint16(stIndex),
	}
}

func VarSet(cvIndex int) Instr {
	return Instr{
		Name:     []byte("var-set"),
		Encoding: AttrEnc1,
		Input:    AttrTake1,
		Data:     uint16(cvIndex),
	}
}

func Discard(n int) Instr {
	return Instr{
		Name:     []byte("discard"),
		Encoding: AttrEnc1,
		Input:    AttrTakeN,
		Data:     uint16(n),
	}
}

func ConstRef(cvIndex int) Instr {
	return Instr{
		Name:     []byte("constant"),
		Encoding: AttrEnc1,
		Output:   AttrPushConst,
		Data:     uint16(cvIndex),
	}
}

func StackRef(stIndex int) Instr {
	return Instr{
		Name:     []byte("stack-ref"),
		Encoding: AttrEnc1,
		Output:   AttrDupNth,
		Data:     uint16(stIndex),
	}
}

func VarRef(cvIndex int) Instr {
	return Instr{
		Name:     []byte("var-ref"),
		Encoding: AttrEnc1,
		Output:   AttrPushTmp,
		Data:     uint16(cvIndex),
	}
}

func Call(argc int) Instr {
	return Instr{
		Name:     []byte("call"),
		Encoding: AttrEnc1,
		Input:    AttrTakeNplus1,
		Output:   AttrPushTmp,
		Data:     uint16(argc),
	}
}
