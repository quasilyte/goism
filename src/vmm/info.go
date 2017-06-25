package vmm

// FuncIsThrowing returns true if function with given name is known
// to be "throwing" (interrupts normal execution flow).
func FuncIsThrowing(name string) bool {
	return throwingFuncs[name]
}

// InstrCallCost returns special function call cost.
// Zero value means that function has no dedicated instruction available.
func InstrCallCost(name string) int {
	return instrNameToCost[name]
}

var throwingFuncs = map[string]bool{
	"goism-rt.Panic": true,
	"error":          true,
	"throw":          true,
	"signal":         true,
}

var instrNameToCost = map[string]int{
	"cons":      2,
	"car":       1,
	"cdr":       1,
	"aref":      1,
	"aset":      1,
	"=":         1,
	">":         1,
	"<":         1,
	"<=":        1,
	">=":        1,
	"+":         1,
	"-":         1,
	"*":         1,
	"/":         2,
	"min":       1,
	"string=":   2,
	"string<":   2,
	"length":    1,
	"not":       1,
	"memq":      2,
	"member":    2,
	"integerp":  1,
	"stringp":   1,
	"symbolp":   1,
	"eq":        1,
	"equal":     1,
	"concat":    6,
	"list":      3,
	"substring": 4,
}
