package conformance

import (
	"strconv"
	"testing"
	"tst/goism"
)

const pkg = "conformance"

func evalCall(call string) string { return goism.EvalCall(pkg, call) }
func evalVar(name string) string  { return goism.EvalVar(pkg, name) }

func testCalls(t *testing.T, table goism.CallTests) {
	goism.RunTestCalls(pkg, t, table)
}

// Enclose each passed string value into double quotes (unconditionnaly).
func q(xs ...string) []string {
	for i, x := range xs {
		xs[i] = `"` + x + `"`
	}
	return xs
}

func chr(ch rune) string {
	return strconv.Itoa(int(ch))
}
