package dt_test

import (
	"dt"
	"testing"
	"tst"
)

func pushN(st *dt.DataStack, n int) {
	for i := 0; i < n; i++ {
		st.Push()
	}
}

func pushVar(st *dt.DataStack, name string) {
	st.Push()
	st.Bind(name)
}

func TestDataStackLen(t *testing.T) {
	if dt.NewDataStack([]string{"x", "y"}).Len() != 2 {
		t.Error(`NewDataStack("x", "y").Len() != 2`)
	}

	st := dt.DataStack{}
	n := 4
	pushN(&st, n)
	tst.CheckError(t, "Len", st.Len(), n)
}

func TestDataStackDiscard(t *testing.T) {
	n := 9
	table := []struct {
		discardN    int
		lenExpected int
	}{
		{0, n},
		{1, n - 1},
		{n - 1, 1},
		{n, 0},
	}

	for _, row := range table {
		st := dt.DataStack{}
		pushN(&st, n)
		st.Discard(uint16(row.discardN))
		tst.CheckError(t, "Len", st.Len(), row.lenExpected)
	}
}

func TestDataStackMaxLen(t *testing.T) {
	table := []struct {
		pushN          int
		discardN       int
		maxLenExpected int
	}{
		{1, 0, 1}, // len=1
		{0, 1, 1}, // len=0
		{2, 0, 2}, // len=2
		{1, 0, 3}, // len=3
		{0, 1, 3}, // len=2
		{4, 1, 6}, // len=5
		{0, 5, 6}, // len=5
		{6, 0, 6}, // len=6
	}

	st := dt.DataStack{}
	for _, row := range table {
		pushN(&st, row.pushN)
		st.Discard(uint16(row.discardN))
		if st.MaxLen() != row.maxLenExpected {
			tst.Errorf(t, "MaxLen", st.MaxLen(), row.maxLenExpected)
		}
	}
}

func TestDataStackLookup(t *testing.T) {
	st := dt.NewDataStack([]string{"x", "y"})
	if st.Lookup("x") != 1 || st.Lookup("y") != 0 {
		t.Error("Lookup for bound names failed")
	}
	if st.Lookup("missing_entry") != -1 {
		t.Error("Lookup for missing entry did not return -1")
	}
}

func TestDataStackDup(t *testing.T) {
	st := dt.NewDataStack([]string{"x"})
	st.Dup(0) // [x x]
	st.Push() // [x x ?]
	st.Dup(1) // [x x ? x]
	tst.CheckError(t, "Len", st.Len(), 4)
	if st.Lookup("x") != 0 {
		t.Error("Lookup of duplicated element failed")
	}
}

func TestDataStackBind(t *testing.T) {
	table := []struct {
		varName string
		lookup  []string
		indexes []int
	}{
		{"a", []string{"bottom", "a"}, []int{1, 0}},
		{"b", []string{"bottom", "a", "b"}, []int{2, 1, 0}},
		{"c", []string{"c", "b", "c"}, []int{0, 1, 0}},
	}

	st := dt.NewDataStack([]string{"bottom"})
	for _, row := range table {
		pushVar(st, row.varName)
		for i, key := range row.lookup {
			tst.CheckError(t, "Lookup", st.Lookup(key), row.indexes[i])
		}
	}
}

func TestDataStackRebind(t *testing.T) {
	st := dt.NewDataStack([]string{"x", "y"})
	st.Rebind(0, "index0")
	st.Rebind(1, "index1")
	if st.Lookup("index0") != 0 || st.Lookup("index1") != 1 {
		t.Error("Lookup of re-bound elements failed")
	}
}

func TestDataStackReplace(t *testing.T) {
	st := dt.NewDataStack([]string{"x", "y", "z"})
	st.Push()     // [x y z ?]
	st.Replace(1) // [x y ?]
	st.Dup(0)     // [x y ? ?]
	st.Replace(3) // [? y ?]
	if st.Lookup("x") != -1 || st.Lookup("z") != -1 {
		t.Error("Replaced elements are found somehow")
	}
	if st.Lookup("y") != 1 {
		t.Error("Can not lookup untouched by Replace() element")
	}
}
