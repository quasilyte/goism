package bytecode

import (
	"bench"
	"testing"
)

// Input data
var (
	ints   = bench.RandIntSlice()
	floats = bench.RandFloatSlice()
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
