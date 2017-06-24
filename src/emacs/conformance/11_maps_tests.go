package conformance

func testMapMake(n int) int {
	xs := make(map[string]string, 2)
	if len(xs) == 0 {
		return n
	}
	return -1
}

func testMapNilLookup(n int) int {
	// Spec: nil map acts like a empty map for lookup.
	var xs map[string]string
	if xs["foo"] == "" {
		return n
	}
	return -1
}

func testMapUpdate(n int) int {
	xs := make(map[int]int, 2)
	xs[150] = n
	xs[275] = n * 2
	if xs[150] == n && xs[275] == n*2 {
		return n
	}
	return -1
}
