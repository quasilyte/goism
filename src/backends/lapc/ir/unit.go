package ir

type Unit struct {
	Instrs []Instr

	lastLabelID int32
	userLabels  map[string]Instr
}

type (
	InstrPusher Unit
)

func NewUnit() *Unit {
	return &Unit{
		Instrs:      make([]Instr, 0, 128),
		lastLabelID: -1,
		userLabels:  make(map[string]Instr),
	}
}

func (u *Unit) Reset() {
	u.Instrs = u.Instrs[:0]
	u.lastLabelID = -1
	u.userLabels = make(map[string]Instr)
}

func (u *Unit) InstrPusher() *InstrPusher {
	return (*InstrPusher)(u)
}

func (u *Unit) NewInlineRetLabel() Instr {
	u.lastLabelID++
	return Instr{Kind: XinlineRetLabel, Data: u.lastLabelID, Meta: "iife-ret"}
}

func (u *Unit) NewLabel(name string) Instr {
	u.lastLabelID++
	return Instr{Kind: Label, Data: u.lastLabelID, Meta: name}
}

func (u *Unit) NewUserLabel(name string) Instr {
	if label, ok := u.userLabels[name]; ok {
		return label
	}
	label := u.NewLabel(name)
	u.userLabels[name] = label
	return label
}

func (p *InstrPusher) PushInstr(ins Instr) {
	p.Instrs = append(p.Instrs, ins)
}

func (p *InstrPusher) push(kind InstrKind) {
	p.PushInstr(Instr{Kind: kind})
}

func (p *InstrPusher) pushMeta(kind InstrKind, meta string) {
	p.PushInstr(Instr{Kind: kind, Meta: meta})
}

func (p *InstrPusher) pushData(kind InstrKind, data int) {
	p.PushInstr(Instr{Kind: kind, Data: int32(data)})
}

func (p *InstrPusher) pushLabel(kind InstrKind, label Instr) {
	p.PushInstr(Instr{Kind: kind, Data: label.Data, Meta: label.Meta})
}

func (p *InstrPusher) Empty(comment string)   { p.pushMeta(Empty, comment) }
func (p *InstrPusher) XvarRef(name string)    { p.pushMeta(XvarRef, name) }
func (p *InstrPusher) XvarSet(name string)    { p.pushMeta(XvarSet, name) }
func (p *InstrPusher) XlocalRef(name string)  { p.pushMeta(XlocalRef, name) }
func (p *InstrPusher) XlocalSet(name string)  { p.pushMeta(XlocalSet, name) }
func (p *InstrPusher) Xbind(name string)      { p.pushMeta(Xbind, name) }
func (p *InstrPusher) ScopeEnter()            { p.push(ScopeEnter) }
func (p *InstrPusher) ScopeLeave()            { p.push(ScopeLeave) }
func (p *InstrPusher) Xgoto(label Instr)      { p.pushLabel(Xgoto, label) }
func (p *InstrPusher) XinlineEnter()          { p.push(XinlineEnter) }
func (p *InstrPusher) XinlineRet(label Instr) { p.pushLabel(XinlineRet, label) }

func (p *InstrPusher) Label(label Instr)            { p.PushInstr(label) }
func (p *InstrPusher) Jmp(label Instr)              { p.pushLabel(Jmp, label) }
func (p *InstrPusher) JmpNil(label Instr)           { p.pushLabel(JmpNil, label) }
func (p *InstrPusher) JmpNotNil(label Instr)        { p.pushLabel(JmpNotNil, label) }
func (p *InstrPusher) JmpNilElsePop(label Instr)    { p.pushLabel(JmpNilElsePop, label) }
func (p *InstrPusher) JmpNotNilElsePop(label Instr) { p.pushLabel(JmpNotNilElsePop, label) }

func (p *InstrPusher) Return() { p.push(Return) }
func (p *InstrPusher) Call(argc int, name string) {
	p.PushInstr(Instr{Kind: Call, Data: int32(argc), Meta: name})
}

func (p *InstrPusher) Eq()        { p.push(Eq) }
func (p *InstrPusher) Equal()     { p.push(Equal) }
func (p *InstrPusher) Substring() { p.push(Substring) }
func (p *InstrPusher) Length()    { p.push(Length) }

func (p *InstrPusher) NumEq()  { p.push(NumEq) }
func (p *InstrPusher) NumLt()  { p.push(NumLt) }
func (p *InstrPusher) NumGt()  { p.push(NumGt) }
func (p *InstrPusher) NumLte() { p.push(NumLte) }
func (p *InstrPusher) NumGte() { p.push(NumGte) }
func (p *InstrPusher) Add()    { p.push(Add) }
func (p *InstrPusher) Sub()    { p.push(Sub) }
func (p *InstrPusher) Mul()    { p.push(Mul) }
func (p *InstrPusher) Quo()    { p.push(Quo) }
func (p *InstrPusher) Add1()   { p.push(Add1) }
func (p *InstrPusher) Sub1()   { p.push(Sub1) }
func (p *InstrPusher) Min()    { p.push(Min) }
func (p *InstrPusher) Neg()    { p.push(Neg) }

func (p *InstrPusher) StrEq()          { p.push(StrEq) }
func (p *InstrPusher) StrLt()          { p.push(StrLt) }
func (p *InstrPusher) Concat(argc int) { p.pushData(Concat, argc) }

func (p *InstrPusher) Aref() { p.push(Aref) }
func (p *InstrPusher) Aset() { p.push(Aset) }

func (p *InstrPusher) Car()       { p.push(Car) }
func (p *InstrPusher) Cdr()       { p.push(Cdr) }
func (p *InstrPusher) SetCar()    { p.push(SetCar) }
func (p *InstrPusher) SetCdr()    { p.push(SetCdr) }
func (p *InstrPusher) Cons()      { p.push(Cons) }
func (p *InstrPusher) List(n int) { p.pushData(List, n) }
func (p *InstrPusher) Memq()      { p.push(Memq) }
func (p *InstrPusher) Member()    { p.push(Member) }

func (p *InstrPusher) Stringp()  { p.push(Stringp) }
func (p *InstrPusher) Integerp() { p.push(Integerp) }
func (p *InstrPusher) Symbolp()  { p.push(Symbolp) }
func (p *InstrPusher) Not()      { p.push(Not) }

func (p *InstrPusher) ConstRef(cvIndex int) { p.pushData(ConstRef, cvIndex) }
func (p *InstrPusher) StackRef(stIndex int) { p.pushData(StackRef, stIndex) }
func (p *InstrPusher) StackSet(stIndex int) { p.pushData(StackSet, stIndex) }
func (p *InstrPusher) Discard(n int)        { p.pushData(Discard, n) }
func (p *InstrPusher) VarRef(cvIndex int)   { p.pushData(VarRef, cvIndex) }
func (p *InstrPusher) VarSet(cvIndex int)   { p.pushData(VarSet, cvIndex) }
