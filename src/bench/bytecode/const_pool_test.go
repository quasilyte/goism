package bytecode

import (
	"bench"
	"testing"
)

// Input data
var (
	ints = []int64{
		1, 2, 3, 1, 3, 4, 6, 4, 3, 4, 2, 1, 2, 2,
		9, 9, 10, 11, 12, 13, 14, 15, 16,
		1, 1, 1, 1, 1, 1, 2, 3, 10, 14, 10,
		50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
		1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4,
	}
	floats = []float64{
		1, 2, 3, 1, 3, 4, 6, 4, 3, 4, 2, 1, 2, 2,
		9, 9, 10, 11, 12, 13, 14, 15, 16,
		1, 1, 1, 1, 1, 1, 2, 3, 10, 14, 10,
		50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
		1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4,
	}
)

type constPool struct {
	size      int
	intBucket struct {
		vals []int64
		keys []int
	}
	floatBucket struct {
		vals []float64
		keys []int
	}
}

func (cp *constPool) insertInt(x int64) int {
	buck := &cp.intBucket

	for i, val := range buck.vals {
		if val == x {
			return buck.keys[i]
		}
	}

	key := cp.size
	cp.size++
	buck.keys = append(buck.keys, key)
	buck.vals = append(buck.vals, x)
	return key
}

func (cp *constPool) insertFloat(x float64) int {
	buck := &cp.floatBucket

	for i, val := range buck.vals {
		if val == x {
			return buck.keys[i]
		}
	}

	key := cp.size
	cp.size++
	buck.keys = append(buck.keys, key)
	buck.vals = append(buck.vals, x)
	return key
}

func BenchmarkConstPool(b *testing.B) {
	bench.Run(b, func(score *int) {
		pool := constPool{}
		for _, x := range ints {
			*score += pool.insertInt(x)
		}
		for _, y := range floats {
			*score += pool.insertFloat(y)
		}
	})
}

type constMap struct {
	size   int
	ints   map[int64]int
	floats map[float64]int
}

func (cm *constMap) insertInt(x int64) int {
	buck := cm.ints

	if key, ok := buck[x]; ok {
		return key
	}

	key := cm.size
	cm.size++
	buck[x] = key
	return key
}

func (cm *constMap) insertFloat(x float64) int {
	buck := cm.floats

	if key, ok := buck[x]; ok {
		return key
	}

	key := cm.size
	cm.size++
	buck[x] = key
	return key
}

func BenchmarkConstMap(b *testing.B) {
	bench.Run(b, func(score *int) {
		pool := constMap{
			ints:   make(map[int64]int),
			floats: make(map[float64]int),
		}
		for _, x := range ints {
			*score += pool.insertInt(x)
		}
		for _, y := range floats {
			*score += pool.insertFloat(y)
		}
	})
}

type constList struct {
	vals []interface{}
}

func (cl *constList) insertInt(x int64) int {
	for i, val := range cl.vals {
		if val == x {
			return i
		}
	}

	key := len(cl.vals)
	cl.vals = append(cl.vals, x)
	return key
}

func (cl *constList) insertFloat(x float64) int {
	for i, val := range cl.vals {
		if val == x {
			return i
		}
	}

	key := len(cl.vals)
	cl.vals = append(cl.vals, x)
	return key
}

func BenchmarkConstList(b *testing.B) {
	bench.Run(b, func(score *int) {
		pool := constList{}
		for _, x := range ints {
			*score += pool.insertInt(x)
		}
		for _, y := range floats {
			*score += pool.insertFloat(y)
		}
	})
}
