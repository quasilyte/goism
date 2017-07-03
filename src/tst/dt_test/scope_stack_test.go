package dt_test

import (
	"dt"
	"testing"
	"tst"
)

func TestScopeEmpty(t *testing.T) {
	var depth int
	defer func() {
		if r := recover(); r != nil {
			// Passed.
		} else {
			tst.Errorf(t, "PopScope()", depth, "panic")
		}
	}()
	ss := dt.ScopeStack{}
	depth = ss.PopScope() // Must panic
}

func TestScopeStackSingle(t *testing.T) {
	ss := dt.ScopeStack{}
	ss.PushScope()
	ss.SetScopeDepth(10)
	if res := ss.PopScope(); res != 10 {
		tst.Errorf(t, "PopScope", res, 10)
	}
}

func TestScopeStackMulti(t *testing.T) {
	table := []struct {
		setDepth bool
		depth    int
	}{
		{true, 1},
		{true, 3},
		{true, 5},
		{false, 0},
		{true, 0},
		{true, 7},
	}

	ss := dt.ScopeStack{}
	for _, row := range table {
		ss.PushScope()
		if row.setDepth {
			ss.SetScopeDepth(row.depth)
		}
	}
	// Pop in reverse order.
	for i := len(table) - 1; i >= 0; i-- {
		row := table[i]
		tst.CheckError(t, "PopScope", ss.PopScope(), row.depth)
	}
}
