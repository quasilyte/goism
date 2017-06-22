package conformance

func sumArray1() int {
	sum, xs := 0, [...]int{1, 2, 3}
	for i := range xs {
		sum += xs[i]
	}
	return sum
}
