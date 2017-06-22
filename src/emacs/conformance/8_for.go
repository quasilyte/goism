package conformance

// Test traditional "for" loop.
func count5() int {
	x := 0
	for i := 0; i < 5; i++ {
		x++
	}
	return x
}

// Test while-like loop.
func countLt10(i int) int {
	x := 0
	for i < 10 {
		x, i = x+1, i+1
	}
	return x
}

// Test break.
func count1() int {
	x := 0
	for {
		x++
		break
	}
	return x
}

// Test continue.
func count2() int {
	x := 0
	for i := 0; i < 10; i += 2 {
		if !(i == 0 || i == 2) {
			continue
		}
		x++
	}
	return x
}

// Test nested loops.
func count20() int {
	x := 0
	for i := 0; i < 5; i++ {
		for i := 0; i < 4; i++ {
			x++
		}
	}
	return x
}

// Test break in nested loops.
func countN(n int) int {
	x := 0
	for i := 0; i < n; i++ {
		for {
			x--
			break
		}
		x += 2
	}
	return x
}

// Test continue in nested loops.
func countMN(m, n int) int {
	x := 0
	for i := 0; i < m*n; i++ {
		x++
		if x < 99999 {
			continue
		}
		x++
	}
	return x
}
