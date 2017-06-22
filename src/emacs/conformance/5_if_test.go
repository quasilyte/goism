package conformance

func testIfTrue(n int) int {
	if true {
		return n
	}
	return -1
}

func testIfFalse(n int) int {
	if false {
		return -1
	}
	return n
}

func testIfZero(x int) bool {
	if x == 0 {
		return true
	}
	return false
}

func testIfElse1(x int) string {
	if x == 0 {
		return "0"
	} else if x == 1 {
		return "1"
	} else if x == 2 {
		return "2"
	}
	return "x"
}

func testIfElse2(x int) string {
	// Uses initializer SimpleStatement;
	// Also includes explicit "else" branch.
	if v := x; v == 0 {
		return "0"
	} else if v == 1 {
		return "1"
	} else if v == 2 {
		return "2"
	} else { // Linter will trigger. That is OK
		return "x"
	}
}

func testNestedIfZero(x int) bool {
	if x > -1 {
		if x < 1 {
			return true
		}
	}
	return false
}

func testIfInitDef(n int) int {
	if x, y := 1, 2; x == y {
		return x - y
	} else if x, y := n, 2*n; x < y {
		return y - x
	}
	return -1
}

func testIfInitAssign(n int) int {
	x, y := -1, -1
	if x, y = 1, 2; x == y {
		return -1
	} else if x, y = n, 2*n; x < y {
		return y - x
	}
	return -1
}

func testAnd(a, b, c bool) bool {
	return a && b && c
}

func testOr(a, b, c bool) bool {
	return a || b || c
}
