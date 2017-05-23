package compiler

// Used to make invariants more expressive.
func assert(success bool) {
	if !success {
		panic("internal error: assertion failed")
	}
}
