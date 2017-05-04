package sexp

func (atom Var) IsAtom() bool { return false }

func (atom Bool) IsAtom() bool   { return true }
func (atom Char) IsAtom() bool   { return true }
func (atom Int) IsAtom() bool    { return true }
func (atom Float) IsAtom() bool  { return true }
func (atom String) IsAtom() bool { return true }

func (lit *ArrayLit) IsAtom() bool    { return false }
func (lit *QuotedArray) IsAtom() bool { return false }

func (fomr *Bind) IsAtom() bool     { return false }
func (fomr *Assign) IsAtom() bool   { return false }
func (form *FormList) IsAtom() bool { return false }
func (form *Block) IsAtom() bool    { return false }
func (form *If) IsAtom() bool       { return false }
func (form *Return) IsAtom() bool   { return false }

func (op *IntAdd) IsAtom() bool      { return false }
func (op *IntSub) IsAtom() bool      { return false }
func (op *IntMul) IsAtom() bool      { return false }
func (op *IntDiv) IsAtom() bool      { return false }
func (op *IntBitOr) IsAtom() bool    { return false }
func (op *IntBitAnd) IsAtom() bool   { return false }
func (op *IntBitXor) IsAtom() bool   { return false }
func (op *IntRem) IsAtom() bool      { return false }
func (op *IntEq) IsAtom() bool       { return false }
func (op *IntNotEq) IsAtom() bool    { return false }
func (op *IntLt) IsAtom() bool       { return false }
func (op *IntLte) IsAtom() bool      { return false }
func (op *IntGt) IsAtom() bool       { return false }
func (op *IntGte) IsAtom() bool      { return false }
func (op *FloatAdd) IsAtom() bool    { return false }
func (op *FloatSub) IsAtom() bool    { return false }
func (op *FloatMul) IsAtom() bool    { return false }
func (op *FloatDiv) IsAtom() bool    { return false }
func (op *FloatEq) IsAtom() bool     { return false }
func (op *FloatNotEq) IsAtom() bool  { return false }
func (op *FloatLt) IsAtom() bool     { return false }
func (op *FloatLte) IsAtom() bool    { return false }
func (op *FloatGt) IsAtom() bool     { return false }
func (op *FloatGte) IsAtom() bool    { return false }
func (op *Concat) IsAtom() bool      { return false }
func (op *StringEq) IsAtom() bool    { return false }
func (op *StringNotEq) IsAtom() bool { return false }
func (op *StringLt) IsAtom() bool    { return false }
func (op *StringLte) IsAtom() bool   { return false }
func (op *StringGt) IsAtom() bool    { return false }
func (op *StringGte) IsAtom() bool   { return false }

func (call *Call) IsAtom() bool { return false }
