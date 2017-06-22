package conformance

func stringifyInt3(x int) string {
	switch x {
	case 0:
		return "0"
	case 1:
		return "1"
	case 2:
		return "2"
	}
	return "x"
}

func stringifyInt4(x int) string {
	// Uses initializer SimpleStatement;
	// Also uses "default" clause.
	switch v := x; v {
	case 0:
		return "0"
	case 1:
		return "1"
	case 2:
		return "2"
	default:
		return "x"
	}
}
