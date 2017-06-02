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

var ThrowingFuncs = map[string]bool{
	"Go--panic": true,
	"error":     true,
	"throw":     true,
	"signal":    true,
}
