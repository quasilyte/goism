package regress

import (
	"testing"
	"tst/goism"
)

func init() {
	goism.LoadPackage("regress")
}

func TestSelfAssign(t *testing.T) {
	testCalls(t, goism.CallTests{
		"selfAssign1 10": "10",
		"selfAssign2 0":  "0",
		"selfAssign2 10": "10",
	})
}
