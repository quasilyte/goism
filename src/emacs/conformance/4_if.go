package conformance

func alwaysZero() int {
	if true {
		return 0
	}
	return 1
}

func neverZero() int {
	if false {
		return 0
	}
	return 1
}

func isZero(x int) bool {
	if x == 0 {
		return true
	}
	return false
}

func stringifyInt1(x int) string {
	if x == 0 {
		return "0"
	} else if x == 1 {
		return "1"
	} else if x == 2 {
		return "2"
	}
	return "x"
}

func stringifyInt2(x int) string {
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

func and(a, b, c bool) bool {
	return a && b && c
}

func or(a, b, c bool) bool {
	return a || b || c
}
