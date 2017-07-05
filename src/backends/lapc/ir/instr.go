package ir

type InstrKind int32

const (
	Empty InstrKind = iota

	/* X-phase Pseudo instructions */

	XvarRef
	XvarSet
	XlocalRef
	XlocalSet
	Xbind
	XscopeEnter
	XscopeLeave
	Xgoto
	XlambdaEnter
	XlambdaRetLabel
	XlambdaRet

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
	Prev *Instr    // Previous instruction
	Next *Instr    // Next instruction
}

// Remove destroys instruction object by removing it from instruction list.
func (ins *Instr) Remove() {
	ins.Prev.Next = ins.Next
	ins.Next.Prev = ins.Prev
}

// InsertNext adds instruction after current position.
func (ins *Instr) InsertNext(newNext *Instr) {
	newNext.Prev = ins
	newNext.Next = ins.Next
	ins.Next.Prev = newNext
	ins.Next = newNext
}

// InsertPrev adds instruction before current position.
func (ins *Instr) InsertPrev(newPrev *Instr) {
	newPrev.Prev = ins.Prev
	newPrev.Next = ins
	ins.Prev.Next = newPrev
	ins.Prev = newPrev
}

// InsertLeft calls InsertPrev consequentially for each passed instruction.
// E.g.:
// [prev ins next].InsertLeft([a b]) => [prev a b ins next].
func (ins *Instr) InsertLeft(span []*Instr) {
	cur := ins.Prev
	for i := range span {
		cur.InsertNext(span[i])
		cur = span[i]
	}
}

// InsertRight calls InsertNext consequentially for each passed instruction.
// E.g.:
// [prev ins next].InsertRight([a b]) => [prev ins a b next].
func (ins *Instr) InsertRight(span []*Instr) {
	cur := ins
	for i := range span {
		cur.InsertNext(span[i])
		cur = cur.Next
	}
}
