package sexp

var (
	EmptyForm    = &emptyForm{}
	EmptyBlock   = Block(nil)
	ContinueGoto = &Goto{LabelName: "continue"}
	BreakGoto    = &Goto{LabelName: "break"}

	Nil Form = Symbol{Val: "nil"}
)
