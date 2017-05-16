package util

import (
	"flag"
	"fmt"
	"os"
	"regexp"
	"strings"
)

// Parsed command line arguments.
// Accessed by Argv function.
var argv map[string]string

// ProgramInfo holds program metadata. Used to
// output better error/info messages.
type ProgramInfo struct {
	Description string
}

// ArgTemplate describes command line argument.
type ArgTemplate struct {
	// If set to true, error if raised when argument is not
	// set explicitly.
	Req bool
	// If set to true, Help string is parsed for {[\w\|]*} pattern.
	// Value is checked against that pattern.
	Enum bool
	// Default value.
	Init string
	// Argument description.
	Help string
}

// ArgvSchema is a set of command line arguments accepted by application.
type ArgvSchema map[string]ArgTemplate

// Argv fetches command line argument by its name.
// Usable only after ParseArgv is completed.
func Argv(key string) string {
	return argv[key]
}

// ParseArgv parses command line arguments using specified schema.
// Argument values can be accessed via Argv function.
func ParseArgv(programInfo *ProgramInfo, schema ArgvSchema) {
	argv = make(map[string]string)
	pointers := make(map[string]*string)

	for name, info := range schema {
		help := info.Help
		if info.Req {
			help += " (Req)"
		}
		pointers[name] = flag.String(name, info.Init, help)
	}
	flag.Parse()

	if len(os.Args) == 1 {
		Usage(programInfo)
	}

	for name, info := range schema {
		p := pointers[name]

		if info.Req && *p == info.Init {
			Blame("Argument `%s' is required\n", name)
		}
		if info.Enum && !checkEnum(*p, info.Help) {
			Blame("Unexpected `%s' value (see usage)\n", name)
		}

		argv[name] = *p
	}
}

var enumRx = regexp.MustCompile(`\{[\w\|]*\}`)

func checkEnum(val string, help string) bool {
	enum := enumRx.FindString(help)
	enum = enum[1 : len(enum)-1] // Drop '{' and '}'
	for _, variant := range strings.Split(enum, "|") {
		if val == variant {
			return true
		}
	}
	return false
}

// Usage prints usage text and exits with code 0.
func Usage(info *ProgramInfo) {
	fmt.Println(info.Description)
	fmt.Println("Params:")
	flag.PrintDefaults()
	os.Exit(0)
}

// Blame prints message to stderr using fmt.Fprintf
// and then exits with non-zero status code.
func Blame(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, format, args...)
	os.Exit(1)
}

// CheckError will invoke Blame if error is not nil.
func CheckError(err error) {
	if err != nil {
		Blame("Error: %v\n", err)
	}
}
