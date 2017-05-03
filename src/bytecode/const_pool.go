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
