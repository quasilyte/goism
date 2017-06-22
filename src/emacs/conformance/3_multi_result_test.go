package conformance

var (
	r2_1, r2_2          = return2()
	_, r3_2, r3_3       = return3()
	r4_1, _, r4_3, r4_4 = return4()
)

func return2() (string, string) {
	return "a", "b"
}

func return3() (string, string, string) {
	return "a", "b", "c"
}

func return4() (string, string, string, string) {
	return "a", "b", "c", "d"
}
