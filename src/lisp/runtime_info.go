package lisp

// RetVars are used in multi-value returns.
// Every return value, except first one, is bound
// to corresponding global variable.
var RetVars = [...]string{
	1: "Go--ret-2",
	2: "Go--ret-3",
	3: "Go--ret-4",
	4: "Go--ret-5",
	5: "Go--ret-6",
	6: "Go--ret-7",
	7: "Go--ret-8",
}

// IsNoreturn returns true for functions that
// will unconditionally interrupt normal execution flow
// (this also means that there will be no meaningful return
// value at the call site).
func IsNoreturn(name string) bool {
	return noreturnFunctions[name]
}

var noreturnFunctions = map[string]bool{
	"panic":      true,
	"Go--panic":  true,
	"error":      true,
	"throw":      true,
	"signal":     true,
	"kill-emacs": true,
}
