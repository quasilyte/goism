package conformance

// #REFS: 5.

func testArrayLit(n int) int {
	xs := [...]int{n, n + 1, n + 2}
	if xs[0] == n && xs[1] == n+1 && xs[2] == n+2 {
		return n
	}
	return -1
}

// #REFS: 73.
// func testKeyedArrayLit(n int) int {
// 	xs := [...]int{
// 		1: n,
// 		3: n + 1,
// 	}
// 	switch {
// 	case len(xs) != 4:
// 		return -1
// 	case xs[0] != 0:
// 		return -1
// 	case xs[1] != n:
// 		return -1
// 	case xs[2] != 0:
// 		return -1
// 	case xs[3] != n+1:
// 		return -1
// 	default:
// 		return n
// 	}
// }

func testArrayZeroVal(n int) int {
	var xs [2]float64
	var ys [2]int
	var zs [2]string
	switch {
	case !(xs[0] == 0.0 && xs[1] == 0.0):
		return -1
	case !(ys[0] == 0 && ys[1] == 0):
		return -1
	case !(zs[0] == "" && zs[1] == ""):
		return -1
	default:
		return n
	}
}

func testArrayUpdate(n int) int {
	var xs [3]int
	xs[0], xs[2] = n, n+1
	if xs[0] == n && xs[1] == 0 && xs[2] == n+1 {
		return n
	}
	return -1
}

func testArrayCopyOnAssign(n int) int {
	xs := [...]int{n, n * 5}
	ys := xs
	ys[0], ys[1] = 0, 1
	if xs[0] != n || xs[1] != n*5 {
		return -1
	}
	return n
}
