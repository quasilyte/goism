package ir

// InstrKind groups instructions by their properties.
type InstrKind int

// Enumeration on instruction kinds.
// Some "kinds" describe single instruction while
// the kinds like InstrBinOp express group of instructions.
const (
	// InstrBinOp : in=2 data=0 out=1.
	InstrBinOp InstrKind = iota

	InstrRet

	InstrConstRef

	InstrCellRef
	InstrCellSet

	InstrVarRef
	InstrVarSet

	InstrStackRef
	InstrStackSet

	InstrDiscard

	InstrCall
	InstrVoidCall
	InstrPanicCall
)

// Instr is a single IR instruction.
type Instr struct {
	Name []byte
	Data uint16
	Kind InstrKind
}

// Binary ops.
var (
	NumEq  = binOp("num=")
	NumGt  = binOp("num>")
	NumLt  = binOp("num<")
	NumSub = binOp("sub")
	NumAdd = binOp("add")
	NumMul = binOp("mul")
	NumQuo = binOp("quo")
)

// Jump instruction names.
var (
	Jmp       = []byte("goto")
	JmpNil    = []byte("goto-if-nil")
	JmpNotNil = []byte("goto-if-not-nil")
	Label     = []byte("label")
)

var (
	Car    = cellRef("car")
	Cdr    = cellRef("cdr")
	SetCar = cellSet("setcar")
	SetCdr = cellSet("setcdr")
)

func Return(n int) Instr {
	return Instr{
		Data: uint16(n),
		Name: []byte("return"),
		Kind: InstrRet,
	}
}

func ConstRef(cvIndex int) Instr {
	return Instr{
		Name: []byte("constant"),
		Data: uint16(cvIndex),
		Kind: InstrConstRef,
	}
}

func VarRef(ref int) Instr {
	return Instr{
		Name: []byte("var-ref"),
		Data: uint16(ref),
		Kind: InstrVarRef,
	}
}

func VarSet(ref int) Instr {
	return Instr{
		Name: []byte("var-set"),
		Data: uint16(ref),
		Kind: InstrVarSet,
	}
}

func StackRef(ref int) Instr {
	return Instr{
		Name: []byte("stack-ref"),
		Data: uint16(ref),
		Kind: InstrStackRef,
	}
}

func StackSet(ref int) Instr {
	return Instr{
		Name: []byte("stack-set"),
		Data: uint16(ref),
		Kind: InstrStackSet,
	}
}

func Discard(n int) Instr {
	return Instr{
		Name: []byte("discard"),
		Data: uint16(n),
		Kind: InstrDiscard,
	}
}

func Call(argc int) Instr      { return call(argc, InstrCall) }
func VoidCall(argc int) Instr  { return call(argc, InstrVoidCall) }
func PanicCall(argc int) Instr { return call(argc, InstrPanicCall) }
