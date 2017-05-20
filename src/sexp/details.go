package sexp

func (atom Bool) form()   {}
func (atom Int) form()    {}
func (atom Float) form()  {}
func (atom String) form() {}
func (atom Symbol) form() {}

func (lit *ArrayLit) form()    {}
func (lit *QuotedArray) form() {}
func (lit *ArrayIndex) form()  {}
func (lit *ArrayUpdate) form() {}

func (form *Panic) form()  {}
func (form *Bind) form()   {}
func (form *Rebind) form() {}

func (form *TypeAssert) form()     {}
func (form *LispTypeAssert) form() {}
func (form *FormList) form()       {}
func (form *Block) form()          {}
func (form *If) form()             {}
func (form *Return) form()         {}
func (form *While) form()          {}

func (op *BitOr) form()       {}
func (op *BitAnd) form()      {}
func (op *BitXor) form()      {}
func (op *NumAddX) form()     {}
func (op *NumSubX) form()     {}
func (op *NumAdd) form()      {}
func (op *NumSub) form()      {}
func (op *NumMul) form()      {}
func (op *NumQuo) form()      {}
func (op *NumEq) form()       {}
func (op *NumNotEq) form()    {}
func (op *NumLt) form()       {}
func (op *NumLte) form()      {}
func (op *NumGt) form()       {}
func (op *NumGte) form()      {}
func (op *Concat) form()      {}
func (op *StringEq) form()    {}
func (op *StringNotEq) form() {}
func (op *StringLt) form()    {}
func (op *StringLte) form()   {}
func (op *StringGt) form()    {}
func (op *StringGte) form()   {}

func (call *Call) form()          {}
func (form CallStmt) form()       {}
func (form *MultiValueRef) form() {}
func (atom Var) form()            {}
