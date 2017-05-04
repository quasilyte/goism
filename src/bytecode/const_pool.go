package bytecode

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

func (cp *ConstPool) InsertSym(x LispSym) int {
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

func (cp *ConstPool) GetSym(index int) LispSym {
	return cp.vals[index].(LispSym)
}
