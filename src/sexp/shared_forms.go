package sexp

var (
	EmptyForm    = &emptyForm{}
	EmptyStmt    = &FormList{}
	EmptyBlock   = &Block{}
	ContinueGoto = &Goto{LabelName: "continue"}
	BreakGoto    = &Goto{LabelName: "break"}
)
