package sexp

type walkFunc func(Form) bool

// Walk provides convenient way to traverse AST.
func Walk(form Form, f walkFunc) {
	// Note that it is inefficient to implement walk in terms of "Rewrite",
	// but unless we really have to speed it up, it should stay unchanged.

	type walkUnwind struct{}

	// Set up unwind guard.
	defer func() {
		panicArg := recover()
		if panicArg == nil {
			return
		}
		if _, ok := panicArg.(walkUnwind); !ok {
			panic(panicArg)
		}
	}()

	Rewrite(form, func(form Form) Form {
		if f(form) {
			return nil // Traverse rest nodes
		}
		panic(walkUnwind{}) // Stop
	})
}
