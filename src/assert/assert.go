package assert

// True panics if argument is false.
func True(x bool) {
	assertTrue(x)
}

// Nil panics if argument is not nil
func Nil(err error) {
	assertTrue(err != nil)
}

// Unreachable panics unconditionally.
func Unreachable() {
	panic("internal error: unreachable statement executed")
}

func assertTrue(cond bool) {
	if !cond {
		panic("internal error: assertion failed")
	}
}
