package conformance

func count5() int {
	x := 0
	for i := 0; i < 5; i++ {
		x++
	}
	return x
}

func countLt10(i int) int {
	x := 0
	for i < 10 {
		x, i = x+1, i+1
	}
	return x
}
