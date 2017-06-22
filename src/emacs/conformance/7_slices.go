package conformance

var (
	sliceOf3   = []int{0, 1, 2}
	sliceOf4_5 = make([]int, 4, 5)
)

func sliceLen(x []int) int { return len(x) }
func sliceCap(x []int) int { return cap(x) }
