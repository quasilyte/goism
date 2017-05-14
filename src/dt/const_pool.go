package dt

import (
	"bytes"
	"lisp"
	"strconv"
)

type ConstPool struct {
	vals []interface{}
}

func (cp *ConstPool) Clear() {
	cp.vals = cp.vals[:0]
}

func (cp *ConstPool) InsertInt(x int64) int {
	for i, val := range cp.vals {
		if val == x {
			return i
		}
	}

	cp.vals = append(cp.vals, x)
	return len(cp.vals) - 1
}

func (cp *ConstPool) InsertFloat(x float64) int {
	for i, val := range cp.vals {
		if val == x {
			return i
		}
	}

	cp.vals = append(cp.vals, x)
	return len(cp.vals) - 1
}

func (cp *ConstPool) InsertString(x string) int {
	for i, val := range cp.vals {
		if val == x {
			return i
		}
	}

	cp.vals = append(cp.vals, x)
	return len(cp.vals) - 1
}

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

func (cp *ConstPool) Get(index uint16) interface{} {
	return cp.vals[index]
}

func (cp *ConstPool) GetInt(index uint16) int64 {
	return cp.vals[index].(int64)
}

func (cp *ConstPool) GetFloat(index uint16) float64 {
	return cp.vals[index].(float64)
}

func (cp *ConstPool) GetString(index uint16) string {
	return cp.vals[index].(string)
}

func (cp *ConstPool) GetSym(index int) lisp.Symbol {
	return cp.vals[index].(lisp.Symbol)
}

func (cp *ConstPool) String() string {
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
	return buf.String()
}
