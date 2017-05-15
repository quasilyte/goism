package bcode

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

	InstrStackRef
	InstrStackSet
	InstrDrop

	InstrCall
	InstrPanicCall
	InstrVoidCall
)

// Instr is a single instruction that
// can be written to bytecode stream.
type Instr struct {
	Opcode byte
	Data   uint16
	Kind   InstrKind
}

// Opcodes without data.
var (
	NumEq  = binOp(85)
	NumGt  = binOp(86)
	NumLt  = binOp(87)
	NumSub = binOp(90)
	NumAdd = binOp(92)
	NumMul = binOp(95)
	NumQuo = binOp(165)
)

func Return(n int) Instr {
	return Instr{Data: uint16(n), Opcode: 135, Kind: InstrRet}
}

func ConstRef(cvIndex int) Instr {
	return Instr{Data: uint16(cvIndex), Kind: InstrConstRef}
}

func StackRef(stIndex int) Instr {
	return Instr{Data: uint16(stIndex), Kind: InstrStackRef}
}

func StackSet(stIndex int) Instr {
	return Instr{Data: uint16(stIndex), Kind: InstrStackSet}
}

func Drop(n int) Instr {
	return Instr{Data: uint16(n), Kind: InstrDrop}
}

func Call(argc int) Instr {
	return Instr{Data: uint16(argc), Kind: InstrCall}
}

func PanicCall(argc int) Instr {
	return Instr{Data: uint16(argc), Kind: InstrPanicCall}
}

func VoidCall(argc int) Instr {
	return Instr{Data: uint16(argc), Kind: InstrVoidCall}
}
