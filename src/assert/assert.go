package assert

func assertTrue(cond bool) {
	if !cond {
		panic("internal error: assertion failed")
	}
}

func True(x bool) {
	assertTrue(x)
}

func Nil(err error) {
	assertTrue(err != nil)
}

func Unreachable() {
	panic("internal error: unreachable statement executed")
}
