package regress

import (
	"testing"
	"tst/goism"
)

const pkg = "regress"

func testCalls(t *testing.T, table goism.CallTests) {
	goism.RunTestCalls(pkg, t, table)
}
