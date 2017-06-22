package sexp

var (
	EmptyStmt    = &FormList{}
	EmptyBlock   = &Block{}
	ContinueGoto = &Goto{LabelName: "continue"}
	BreakGoto    = &Goto{LabelName: "break"}
)
