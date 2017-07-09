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

func testMapDelete(n int) int {
	xs := make(map[string]int, 1)
	xs["foo"] = 10
	delete(xs, "foo")
	if xs["foo"] == 0 {
		return n
	}
	return -1
}

func testMapLen(n int) int {
	xs := make(map[string]int)
	if len(xs) != 0 {
		return -1
	}
	xs["foo"] = 1 // Insert
	if len(xs) != 1 {
		return -2
	}
	xs["foo"] = 2 // Update
	if len(xs) != 1 {
		return -3
	}
	xs["bar"] = 3
	if len(xs) != 2 {
		return -4
	}
	return n
}
