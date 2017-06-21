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

func evalVar(name string) string {
	return eval("goism-conformance." + name)
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

func TestGlobalVars(t *testing.T) {
	table := []struct {
		name          string
		valueExpected string
	}{
		{"var1", "1"},
		{"var2", "2"},
		{"var3", "3"},
		{"var4", "4"},
		{"var5", "5"},
		{"var6", "6"},
	}

	for _, row := range table {
		res := evalVar(row.name)
		if res != row.valueExpected {
			t.Errorf("%s=>%s (want %s)", row.name, res, row.valueExpected)
		}
	}
}

func TestOps(t *testing.T) {
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
