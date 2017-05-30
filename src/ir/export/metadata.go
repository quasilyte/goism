package export

import (
	"exn"
	"strings"
	"tu"
)

// Return properly encoded bytecode function argument descriptor.
func argsDescriptor(fn *tu.Func) int {
	arity := len(fn.Params)
	if arity > 127 {
		panic(exn.User("can not have more than 127 positional parameters"))
	}

	positionalArgs := uint32(arity) // First 7 bits: required args
	const variadicBit = 128         // 8-th bit: "rest" arg
	totalArgs := uint32(arity << 8) // Other bits

	if fn.Variadic {
		return int(positionalArgs + variadicBit + totalArgs)
	}
	return int(positionalArgs + totalArgs)
}

// Return extended function documentation string.
func docString(fn *tu.Func) string {
	if len(fn.Params) == 0 {
		return fn.DocString
	}

	// Add signature information for eldoc.
	params := append(make([]string, 0, len(fn.Params)), fn.Params...)
	if fn.Variadic {
		params[len(params)-1] = "&rest " + params[len(params)-1]
	}
	eldocSig := "\n(fn " + strings.Join(params, " ") + ")"

	if fn.DocString != "" {
		comment := strings.Replace(fn.DocString, `"`, `\"`, -1)
		return comment + eldocSig
	}
	return "\n" + eldocSig
}
