package bytecode

import (
	"bench"
	"testing"
)

var (
	fewInts    = []int64{1, 1, 4, 5, 10}
	fewFloats  = []float64{1, 1, 4, 5, 10}
	manyInts   = bench.RandInt64Slice()
	manyFloats = bench.RandFloat64Slice()
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

func (cp *constPool) findInt(key int) int64 {
	buck := &cp.intBucket
	for i, x := range buck.keys {
		if key == x {
			return buck.vals[i]
		}
	}
	return 0
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

func cpInsert(score *int, ints []int64, floats []float64) {
	pool := constPool{}
	for _, x := range ints {
		*score += pool.insertInt(x)
	}
	for _, y := range floats {
		*score += pool.insertFloat(y)
	}
}

func cpFind(score *int, ints []int64, n int) {
	pool := constPool{}
	for _, x := range ints {
		pool.insertInt(x)
	}
	for i := 0; i < n; i++ {
		for key := range ints {
			*score += int(pool.findInt(key))
		}
	}
}

func BenchmarkConstPoolInsertFew(b *testing.B) {
	bench.Run(b, func(score *int) {
		cpInsert(score, fewInts, fewFloats)
	})
}
func BenchmarkConstMapInsertFew(b *testing.B) {
	bench.Run(b, func(score *int) {
		cmInsert(score, fewInts, fewFloats)
	})
}
func BenchmarkConstListInsertFew(b *testing.B) {
	bench.Run(b, func(score *int) {
		clInsert(score, fewInts, fewFloats)
	})
}

func BenchmarkConstPoolInsertMany(b *testing.B) {
	bench.Run(b, func(score *int) {
		cpInsert(score, manyInts, manyFloats)
	})
}
func BenchmarkConstMapInsertMany(b *testing.B) {
	bench.Run(b, func(score *int) {
		cmInsert(score, manyInts, manyFloats)
	})
}
func BenchmarkConstListInsertMany(b *testing.B) {
	bench.Run(b, func(score *int) {
		clInsert(score, manyInts, manyFloats)
	})
}

func BenchmarkConstPoolFindFew(b *testing.B) {
	bench.Run(b, func(score *int) {
		cpFind(score, fewInts, 20)
	})
}
func BenchmarkConstMapFindFew(b *testing.B) {
	bench.Run(b, func(score *int) {
		cmFind(score, fewInts, 20)
	})
}
func BenchmarkConstListFindFew(b *testing.B) {
	bench.Run(b, func(score *int) {
		clFind(score, fewInts, 20)
	})
}

func BenchmarkConstPoolFindMany(b *testing.B) {
	bench.Run(b, func(score *int) {
		cpFind(score, manyInts, 10)
	})
}
func BenchmarkConstMapFindMany(b *testing.B) {
	bench.Run(b, func(score *int) {
		cmFind(score, manyInts, 10)
	})
}
func BenchmarkConstListFindMany(b *testing.B) {
	bench.Run(b, func(score *int) {
		clFind(score, manyInts, 20)
	})
}

type constMap struct {
	size int

	intSet map[int64]int
	ints   []int64

	floatSet map[float64]int
	floats   []float64
}

func (cm *constMap) insertInt(x int64) int {
	if key, ok := cm.intSet[x]; ok {
		return key
	}

	key := cm.size
	cm.size++
	cm.intSet[x] = key
	cm.ints = append(cm.ints, x)
	return key
}

func (cm *constMap) insertFloat(x float64) int {
	if key, ok := cm.floatSet[x]; ok {
		return key
	}

	key := cm.size
	cm.size++
	cm.floatSet[x] = key
	cm.floats = append(cm.floats, x)
	return key
}

func (cm *constMap) findInt(key int) int64 {
	if key >= len(cm.ints) {
		return 0
	}
	return cm.ints[key]
}

func cmInsert(score *int, ints []int64, floats []float64) {
	pool := constMap{
		intSet:   make(map[int64]int),
		floatSet: make(map[float64]int),
	}
	for _, x := range ints {
		*score += pool.insertInt(x)
	}
	for _, y := range floats {
		*score += pool.insertFloat(y)
	}
}

func cmFind(score *int, ints []int64, n int) {
	pool := constMap{
		intSet:   make(map[int64]int),
		floatSet: make(map[float64]int),
	}
	for _, x := range ints {
		pool.insertInt(x)
	}
	for i := 0; i < n; i++ {
		for key := range ints {
			*score += int(pool.findInt(key))
		}
	}
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

func (cl *constList) findInt(key int) int64 {
	if key >= len(cl.vals) {
		return 0
	}
	return cl.vals[key].(int64)
}

func clInsert(score *int, ints []int64, floats []float64) {
	pool := constList{}
	for _, x := range ints {
		*score += pool.insertInt(x)
	}
	for _, y := range floats {
		*score += pool.insertFloat(y)
	}
}

func clFind(score *int, ints []int64, n int) {
	pool := constList{}
	for _, x := range ints {
		pool.insertInt(x)
	}
	for i := 0; i < n; i++ {
		for key := range ints {
			*score += int(pool.findInt(key))
		}
	}
}
