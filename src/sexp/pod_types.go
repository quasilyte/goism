package sexp

// CaseClause is a part of SwitchBody.
type CaseClause struct {
	Expr Form
	Body Block
}

// SwitchBody represents switch statement case sequence
// with optional default clause.
type SwitchBody struct {
	Clauses     []CaseClause
	DefaultBody Block // Can be EmptyBlock (no default clause)
}
