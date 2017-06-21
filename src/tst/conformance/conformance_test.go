package conformance

import (
	"os"
	"os/exec"
	"strings"
	"testing"
)

var home = os.Getenv("GOISM_DIR")

func evalCall(call string) string {
	return eval("(goism-conformance." + call + ")")
}

func eval(expr string) string {
	out, err := exec.Command(home+"/script/tst/daemon_eval", expr).Output()
	if err != nil {
		return "error: " + err.Error()
	}
	return strings.TrimRight(string(out), "\t\n\r")
}

func init() {
	// Loads "emacs/conformance" package into Emacs daemon.
	eval(`(goism-load "conformance")`)
}

func TestBinOps(t *testing.T) {
	table := []struct {
		call           string
		outputExpected string
	}{
		// Int ops.
		{"add1Int 1", "2"},
		{"addInt 1 2 3", "6"},
		{"sub1Int 1", "0"},
		{"subInt 3 2 1", "0"},
		// Float ops.
		{"add1Float 1.0", "2.0"},
		{"addFloat 1.1 2.2 3.3", "6.6"},
		{"sub1Float 1.5", "0.5"},
		{"subFloat 3.5 2.5 1.5", "-0.5"},
	}

	for _, row := range table {
		res := evalCall(row.call)
		if res != row.outputExpected {
			t.Errorf("(%s)=>%s (want %s)", row.call, res, row.outputExpected)
		}
	}
}
