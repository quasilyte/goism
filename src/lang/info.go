package lang

var throwingFuncs = map[string]bool{
	"goism-rt.Panic": true,
	"error":          true,
	"throw":          true,
	"signal":         true,
}

// FuncIsThrowing returns true if function with given name is known
// to be "throwing" (interrupts normal execution flow).
func FuncIsThrowing(name string) bool {
	return throwingFuncs[name]
}
