package pairwise

type s1 struct{ m1 int }
type s2 struct{ m1, m2 int }
type s5 struct{ m1, m2, m3, m4, m5 int }

func testStructIndex1() int {
	x := s1{m1: 1}
	return x.m1
}

func testStructIndex2() int {
	x := s2{m1: 1, m2: 2}
	return x.m1 + x.m2
}

func testStructIndex5() int {
	x := s5{m1: 1, m2: 2, m3: 3, m4: 4, m5: 5}
	return x.m1 + x.m2 + x.m3 + x.m4 + x.m5
}

func copyS1(x s1) s1 { return x }
func copyS2(x s2) s2 { return x }
func copyS5(x s5) s5 { return x }

func testStructCopy1() int {
	x := s1{m1: 1}
	y := x
	y.m1 = -1
	z := copyS1(x)
	z.m1 = -1
	return x.m1
}

func testStructCopy2() int {
	x := s2{m1: 1, m2: 2}
	y := x
	y.m1, y.m2 = -1, -2
	z := copyS2(x)
	z.m1, z.m2 = -1, -2
	return x.m1 + x.m2
}

func testStructCopy5() int {
	x := s5{m1: 1, m2: 2, m3: 3, m4: 4, m5: 5}
	y := x
	y.m1, y.m2, y.m3, y.m4, y.m5 = -1, -2, -3, -4, -5
	z := copyS5(x)
	z.m1, z.m2, z.m3, z.m4, z.m5 = -1, -2, -3, -4, -5
	return x.m1 + x.m2 + x.m3 + x.m4 + x.m5
}

func testStructUpdate1() int {
	x := &s1{m1: 1}
	x.m1 = -1
	return x.m1
}

func testStructUpdate2() int {
	x := &s2{m1: 1, m2: 2}
	x.m1, x.m2 = -1, -2
	return x.m1 + x.m2
}

func testStructUpdate5() int {
	x := &s5{m1: 1, m2: 2, m3: 3, m4: 4, m5: 5}
	x.m1, x.m2, x.m3, x.m4, x.m5 = -1, -2, -3, -4, -5
	return x.m1 + x.m2 + x.m3 + x.m4 + x.m5
}

func testStructPtrMutate1() int {
	x := &s1{m1: 1}
	y := x
	y.m1 = -1
	return x.m1
}

func testStructPtrMutate2() int {
	x := &s2{m1: 1, m2: 2}
	y := x
	y.m1, y.m2 = -1, -2
	return x.m1 + x.m2
}

func testStructPtrMutate5() int {
	x := &s5{m1: 1, m2: 2, m3: 3, m4: 4, m5: 5}
	y := x
	y.m1, y.m2, y.m3, y.m4, y.m5 = -1, -2, -3, -4, -5
	return x.m1 + x.m2 + x.m3 + x.m4 + x.m5
}

type firster interface {
	first() int
	firstNeg() int
}

func (x *s1) first() int { return x.m1 }
func (x *s2) first() int { return x.m1 }
func (x *s5) first() int { return x.m1 }

func (x *s1) firstNeg() int { return -x.first() }
func (x *s2) firstNeg() int { return -x.first() }
func (x *s5) firstNeg() int { return -x.first() }

func testMethod1() int {
	x := &s1{m1: 1}
	return x.first()
}

func testMethod2() int {
	x := &s2{m1: 1, m2: 2}
	return x.first()
}

func testMethod5() int {
	x := &s5{m1: 1, m2: 2, m3: 3, m4: 4, m5: 5}
	return x.first()
}

func testIfaceArray() int {
	xs := [...]firster{
		&s1{m1: 1},
		&s2{m1: 2},
		&s5{m1: 3},
	}
	res := 0
	for i := range xs {
		res += xs[i].first() + xs[i].firstNeg()
	}
	return res
}

func testIfaceAssign() int {
	var x, y, z firster = &s1{m1: 1}, &s2{m1: 2}, &s5{m1: 3}
	a, b, c := x, y, z
	return a.first() + b.firstNeg() + c.first()
}

func useFirster(x firster) int { return x.first() + x.firstNeg() }

func testIfacePassLiteral() int {
	return useFirster(&s1{m1: 1}) +
		useFirster(&s2{m1: 2}) +
		useFirster(&s5{m1: 3})
}

func testIfacePassVar() int {
	x, y, z := &s1{m1: 1}, &s2{m1: 2}, &s5{m1: 3}
	return useFirster(x) +
		useFirster(y) +
		useFirster(z)
}
