package bench

import "testing"

// Used for benchmark result assignment
// (to disappoint Go optimizer).
var gScore int

// Run executes given function b.N times
// and collects the result (score).
// Score pointer is passed into benchmarking function.
func Run(b *testing.B, bench func(*int)) {
	score := 0
	for i := 0; i < b.N; i++ {
		bench(&score)
	}
	gScore = score
}
