package sexp

func (atom Var) IsAtom() bool { return false }

func (atom Bool) IsAtom() bool   { return true }
func (atom Char) IsAtom() bool   { return true }
func (atom Int) IsAtom() bool    { return true }
func (atom Float) IsAtom() bool  { return true }
func (atom String) IsAtom() bool { return true }
func (atom Symbol) IsAtom() bool { return true }

func (lit *ArrayLit) IsAtom() bool    { return false }
func (lit *QuotedArray) IsAtom() bool { return false }

func (fomr *Bind) IsAtom() bool     { return false }
func (fomr *Rebind) IsAtom() bool   { return false }
func (form ExprStmt) IsAtom() bool  { return false }
func (form *FormList) IsAtom() bool { return false }
func (form *Block) IsAtom() bool    { return false }
func (form *If) IsAtom() bool       { return false }
func (form *Return) IsAtom() bool   { return false }

func (op MakeMap) IsAtom() bool      { return false }
func (op *MapSet) IsAtom() bool      { return false }
func (op *BitOr) IsAtom() bool       { return false }
func (op *BitAnd) IsAtom() bool      { return false }
func (op *BitXor) IsAtom() bool      { return false }
func (op *NumAdd) IsAtom() bool      { return false }
func (op *NumSub) IsAtom() bool      { return false }
func (op *NumMul) IsAtom() bool      { return false }
func (op *NumQuo) IsAtom() bool      { return false }
func (op *NumEq) IsAtom() bool       { return false }
func (op *NumNotEq) IsAtom() bool    { return false }
func (op *NumLt) IsAtom() bool       { return false }
func (op *NumLte) IsAtom() bool      { return false }
func (op *NumGt) IsAtom() bool       { return false }
func (op *NumGte) IsAtom() bool      { return false }
func (op *Concat) IsAtom() bool      { return false }
func (op *StringEq) IsAtom() bool    { return false }
func (op *StringNotEq) IsAtom() bool { return false }
func (op *StringLt) IsAtom() bool    { return false }
func (op *StringLte) IsAtom() bool   { return false }
func (op *StringGt) IsAtom() bool    { return false }
func (op *StringGte) IsAtom() bool   { return false }

func (call *Call) IsAtom() bool { return false }
