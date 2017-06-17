package sexp

type walkFunc func(Form) bool

// Walk provides convenient way to traverse AST.
func Walk(form Form, f walkFunc) {
	// Note that it is inefficient to implement walk in terms of "Rewrite",
	// but unless we really have to speed it up, it should stay unchanged.
	Rewrite(form, func(form Form) Form {
		if f(form) {
			return nil // Traverse rest nodes
		}
		return form // Stop
	})
}
