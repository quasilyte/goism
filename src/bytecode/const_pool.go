package bytecode

import (
	"bytes"
	"emacs/lisp"
	"strconv"
)

type ConstPool struct {
	vals []interface{}
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

func (cp *ConstPool) InsertSym(x lisp.Symbol) int {
	for i, val := range cp.vals {
		if val == x {
			return i
		}
	}

	cp.vals = append(cp.vals, x)
	return len(cp.vals) - 1
}

func (cp *ConstPool) GetInt(index int) int64 {
	return cp.vals[index].(int64)
}

func (cp *ConstPool) GetFloat(index int) float64 {
	return cp.vals[index].(float64)
}

func (cp *ConstPool) GetString(index int) string {
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
