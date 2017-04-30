package sexp

func (atom Bool) private()   {}
func (atom Int) private()    {}
func (atom Float) private()  {}
func (atom String) private() {}
func (atom Var) private()    {}

func (lit *ArrayLit) private()    {}
func (lit *QuotedArray) private() {}

func (form *Block) private()  {}
func (form *If) private()     {}
func (form *Return) private() {}

func (op *IntAdd) private()          {}
func (op *IntSub) private()          {}
func (op *IntMul) private()          {}
func (op *IntDiv) private()          {}
func (op *IntBitOr) private()        {}
func (op *IntBitAnd) private()       {}
func (op *IntBitXor) private()       {}
func (op *IntRem) private()          {}
func (op *IntEq) private()           {}
func (op *IntNotEq) private()        {}
func (op *IntLess) private()         {}
func (op *IntLessEq) private()       {}
func (op *IntGreater) private()      {}
func (op *IntGreaterEq) private()    {}
func (op *FloatAdd) private()        {}
func (op *FloatSub) private()        {}
func (op *FloatMul) private()        {}
func (op *FloatDiv) private()        {}
func (op *FloatEq) private()         {}
func (op *FloatNotEq) private()      {}
func (op *FloatLess) private()       {}
func (op *FloatLessEq) private()     {}
func (op *FloatGreater) private()    {}
func (op *FloatGreaterEq) private()  {}
func (op *Concat) private()          {}
func (op *StringEq) private()        {}
func (op *StringNotEq) private()     {}
func (op *StringLess) private()      {}
func (op *StringLessEq) private()    {}
func (op *StringGreater) private()   {}
func (op *StringGreaterEq) private() {}

func (call *Call) private() {}
