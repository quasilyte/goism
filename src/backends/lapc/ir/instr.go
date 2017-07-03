package ir

type InstrKind int32

const (
	/* Pseudo instructions */

	Empty InstrKind = iota
	XvarRef
	XvarSet
	XlocalRef
	XlocalSet
	Xbind
	ScopeEnter
	ScopeLeave
	Xgoto
	XinlineEnter
	XinlineRetLabel
	XinlineRet

	/* Emacs VM instructions */

	Label
	Jmp              // "goto"
	JmpNil           // "gotoifnil"
	JmpNotNil        // "gotoifnonnil"
	JmpNilElsePop    // "gotoifnilelsepop"
	JmpNotNilElsePop // "gotoifnonnilelsepop"

	Return
	Call

	Eq
	Equal
	Substring // "substring"
	Length

	NumEq  // "eqlsign"
	NumLt  // "lss"
	NumGt  // "gtr"
	NumLte // "leq"
	NumGte // "geq"
	Add    // "plus"
	Sub    // "diff"
	Mul    // "mult"
	Quo
	Add1
	Sub1
	Min
	Neg // "negate"

	StrEq // "stringeqlsign"
	StrLt // "stringlss"
	Concat

	Aref
	Aset

	Car
	Cdr
	SetCar
	SetCdr
	Cons
	List
	Memq
	Member

	Stringp
	Integerp
	Symbolp
	Not

	ConstRef
	StackRef
	StackSet
	Discard
	VarRef
	VarSet
)

// Instr is a single IR instruction.
type Instr struct {
	Kind InstrKind // Determines the instruction kind
	Data int32     // Instruction direct argument (optional)
	Meta string    // Additional instruction data (optional)
}
