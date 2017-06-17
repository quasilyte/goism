package dt

import (
	"bytes"
	"magic_pkg/emacs/lisp"
	"strconv"
)

// ConstPool is a set of distincs constant values.
// It stores atoms of int, float, string and symbol types.
//
// Serves as a builder for Emacs function constant vector.
type ConstPool struct {
	vals []interface{}
}

// Clear removes all stored elements. Storage is reused.
func (cp *ConstPool) Clear() {
	cp.vals = cp.vals[:0]
}

// InsertInt inserts given argument if it is not already present.
// Returns constant vector index.
func (cp *ConstPool) InsertInt(x int64) int {
	for i, val := range cp.vals {
		if val == x {
			return i
		}
	}

	cp.vals = append(cp.vals, x)
	return len(cp.vals) - 1
}

// InsertFloat inserts given argument if it is not already present.
// Returns constant vector index.
func (cp *ConstPool) InsertFloat(x float64) int {
	for i, val := range cp.vals {
		if val == x {
			return i
		}
	}

	cp.vals = append(cp.vals, x)
	return len(cp.vals) - 1
}

// InsertString inserts given argument if it is not already present.
// Returns constant vector index.
func (cp *ConstPool) InsertString(x string) int {
	for i, val := range cp.vals {
		if val == x {
			return i
		}
	}

	cp.vals = append(cp.vals, x)
	return len(cp.vals) - 1
}

// InsertSym inserts given argument if it is not already present.
// Returns constant vector index.
func (cp *ConstPool) InsertSym(x string) int {
	sym := lisp.Symbol(x)
	for i, val := range cp.vals {
		if val == sym {
			return i
		}
	}

	cp.vals = append(cp.vals, sym)
	return len(cp.vals) - 1
}

// Get extracts constant vector value stored at specified index.
func (cp *ConstPool) Get(index uint16) interface{} {
	return cp.vals[index]
}

// GetInt is like Get, but result is type asserted to int64.
func (cp *ConstPool) GetInt(index uint16) int64 {
	return cp.vals[index].(int64)
}

// GetFloat is like Get, but result is type asserted to float64.
func (cp *ConstPool) GetFloat(index uint16) float64 {
	return cp.vals[index].(float64)
}

// GetString is like Get, but result is type asserted to string.
func (cp *ConstPool) GetString(index uint16) string {
	return cp.vals[index].(string)
}

// GetSym is like Get, but result is type asserted to lisp.Symbol.
func (cp *ConstPool) GetSym(index int) lisp.Symbol {
	return cp.vals[index].(lisp.Symbol)
}

// Bytes returns printed representation of Emacs Lisp constant vector.
func (cp *ConstPool) Bytes() []byte {
	buf := bytes.Buffer{}
	buf.WriteByte('[')
	for _, x := range cp.vals {
		switch x := x.(type) {
		case string:
			buf.WriteByte('"')
			buf.WriteString(x)
			buf.WriteByte('"')
		case int64:
			buf.WriteString(strconv.FormatInt(x, 10))
		case float64:
			buf.WriteString(strconv.FormatFloat(x, 'f', -1, 64))
		case lisp.Symbol:
			buf.WriteString(string(x))
		}
		buf.WriteByte(' ')
	}
	buf.WriteByte(']')
	return buf.Bytes()
}
