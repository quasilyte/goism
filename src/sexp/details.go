package sexp

func (atom Bool) form()   {}
func (atom Int) form()    {}
func (atom Float) form()  {}
func (atom String) form() {}
func (atom Symbol) form() {}

func (lit *ArrayLit) form()       {}
func (lit *SparseArrayLit) form() {}
func (lit *SliceLit) form()       {}

func (form *ArrayIndex) form()  {}
func (form *ArrayUpdate) form() {}
func (form *ArrayCopy) form()   {}
func (form *SliceLen) form()    {}
func (form *SliceCap) form()    {}
func (form *SliceIndex) form()  {}
func (form *SliceUpdate) form() {}
func (form *Subslice) form()    {}

func (form *Panic) form()  {}
func (form *Bind) form()   {}
func (form *Rebind) form() {}

func (form *TypeAssert) form()     {}
func (form *LispTypeAssert) form() {}
func (form *FormList) form()       {}
func (form *Block) form()          {}
func (form *If) form()             {}
func (form *Return) form()         {}
func (form *Repeat) form()         {}
func (form *DoTimes) form()        {}
func (form *While) form()          {}

func (op *Not) form()  {}
func (op *Neg) form()  {}
func (op *AddX) form() {}
func (op *SubX) form() {}

func (op *Shl) form()      {}
func (op *Shr) form()      {}
func (op *BitOr) form()    {}
func (op *BitAnd) form()   {}
func (op *BitXor) form()   {}
func (op *Add) form()      {}
func (op *Sub) form()      {}
func (op *Mul) form()      {}
func (op *Quo) form()      {}
func (op *NumEq) form()    {}
func (op *NumNotEq) form() {}
func (op *NumLt) form()    {}
func (op *NumLte) form()   {}
func (op *NumGt) form()    {}
func (op *NumGte) form()   {}
func (op *StrEq) form()    {}
func (op *StrNotEq) form() {}
func (op *StrLt) form()    {}
func (op *StrLte) form()   {}
func (op *StrGt) form()    {}
func (op *StrGte) form()   {}

func (op *Concat) form() {}

func (call *Call) form()          {}
func (form CallStmt) form()       {}
func (form *MultiValueRef) form() {}
func (v Var) form()               {}
func (form *Let) form()           {}
