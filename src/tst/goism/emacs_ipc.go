package goism

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"
)

// Directory that contains goism repository (sources, scripts, etc).
var home = func() string {
	res := os.Getenv("GOISM_DIR")
	if res == "" {
		panic("GOISM_DIR environment variable is not set")
	}
	return res
}()

var varReplaceRx = regexp.MustCompile(`\$(\w+)`)

// Eval is a fundamental way to communicate with Emacs daemon.
// It sends Lisp S-expression (function argument) to Emacs
// for evaluation. The result is returned as a string.
//
// It does not perform any expression transformation,
// it is passed "as is".
// In practive, it means that you must mangle goism-generated
// symbols by hand.
//
// Also note that this call modifies Emacs daemon state.
// Side effects are preserved between calls.
//
// If error occurs, its message is returned as a result.
// Error message is prepended with "error: " string to
// reduce probability of mixing up erroneous result with correct result.
//
// Example:
// Eval("(+ 1 2)") => "3"
// Eval("nil")     => "nil"
func Eval(expr string) string {
	var (
		stdout bytes.Buffer
		stderr bytes.Buffer
	)

	cmd := exec.Command(home+"/script/tst/daemon_eval", expr)
	cmd.Stderr = &stderr
	cmd.Stdout = &stdout

	err := cmd.Run()
	if err != nil {
		return fmt.Sprintf("error: %s (%v)", stderr.String(), err)
	}
	return strings.TrimRight(string(stdout.String()), "\t\n\r")
}

// LoadPackage loads package of specified name into Emacs daemon.
func LoadPackage(pkg string) {
	Eval(fmt.Sprintf(`(goism-load "%s")`, pkg))
}

// EvalCall runs call evaluates function call expression.
// For convenience, $var arguments are permitted (auto symbol mangling).
//
// Example:
// EvalCall("identity 1") => "1"
// EvalCall("list 1 2 3") => "(1 2 3)"
func EvalCall(pkg string, call string) string {
	call = varReplaceRx.ReplaceAllString(call, "goism-conformance.$1")
	return Eval("(goism-" + pkg + "." + call + ")")
}

// EvalVar evaluates variable and returns its value.
func EvalVar(pkg string, name string) string {
	return Eval("goism-" + pkg + "." + name)
}
