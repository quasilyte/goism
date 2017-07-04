package conformance

func factorial(x int64) int64 {
	if x <= 1 {
		return 1
	}
	return x * factorial(x-1)
}

func isAlpha(c byte) bool {
	return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
}

const minFloat = -1.797693134862315708145274237317043567981e+308

func max4(a, b, c, d float64) float64 {
	max := a
	xs := [...]float64{b, c, d}
	for i := range xs {
		if xs[i] > max {
			max = xs[i]
		}
	}
	return max
}
