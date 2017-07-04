package sexp

var (
	EmptyForm    = &emptyForm{}
	EmptyStmt    = &FormList{}
	EmptyBlock   = Block(nil)
	ContinueGoto = &Goto{LabelName: "continue"}
	BreakGoto    = &Goto{LabelName: "break"}
)
