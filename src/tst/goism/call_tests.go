package goism

import (
	"testing"
	"tst"
)

// CallTests is a simple "call"=>"result" table.
// Used with RunTestCalls.
type CallTests map[string]string

// RunTestCalls runs every function from given CallTests table.
// Every result is checked with tst.CheckError.
func RunTestCalls(pkg string, t *testing.T, table CallTests) {
	for call, outputExpected := range table {
		res := EvalCall(pkg, call)
		tst.CheckError(t, call, res, outputExpected)
	}
}
