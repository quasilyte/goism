package sexp

func (atom Bool) form()   {}
func (atom Int) form()    {}
func (atom Float) form()  {}
func (atom Str) form()    {}
func (atom Symbol) form() {}
func (atom Var) form()    {}

func (lit *ArrayLit) form()       {}
func (lit *SparseArrayLit) form() {}
func (lit *SliceLit) form()       {}
func (lit *StructLit) form()      {}

func (form *ArrayUpdate) form()  {}
func (form *SliceUpdate) form()  {}
func (form *StructUpdate) form() {}
func (form *Bind) form()         {}
func (form *Rebind) form()       {}
func (form *VarUpdate) form()    {}
func (form *FormList) form()     {}
func (form *Block) form()        {}
func (form *If) form()           {}
func (form *Return) form()       {}
func (form *ExprStmt) form()     {}

func (form *Repeat) form()  {}
func (form *DoTimes) form() {}
func (form *While) form()   {}

func (form *ArrayIndex) form()  {}
func (form *SliceIndex) form()  {}
func (form *StructIndex) form() {}

func (form *ArraySlice) form() {}
func (form *SliceSlice) form() {}

func (form *TypeAssert) form() {}

func (call *Call) form()      {}
func (call *LispCall) form()  {}
func (call *InstrCall) form() {}
func (form *Let) form()       {}

func (form *And) form() {}
func (form *Or) form()  {}
