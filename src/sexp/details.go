package sexp

func (atom Bool) form()   {}
func (atom Int) form()    {}
func (atom Float) form()  {}
func (atom Str) form()    {}
func (atom Symbol) form() {}

func (lit *ArrayLit) form()       {}
func (lit *SparseArrayLit) form() {}
func (lit *SliceLit) form()       {}

func (form *ArrayIndex) form()  {}
func (form *ArrayUpdate) form() {}
func (form *ArrayCopy) form()   {}
func (form *ArraySlice) form()  {}
func (form *SliceLen) form()    {}
func (form *SliceCap) form()    {}
func (form *SliceIndex) form()  {}
func (form *SliceUpdate) form() {}
func (form *Subslice) form()    {}
func (form *Substr) form()      {}

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

func (op UnaryOp) form() {}
func (op BinOp) form()   {}

func (call *Call) form()    {}
func (form CallStmt) form() {}
func (v Var) form()         {}
func (form *Let) form()     {}
