package compiler

import (
	"strings"
	"tu"
)

// Return properly encoded bytecode function argument descriptor.
func argsDescriptor(arity int, variadic bool) uint32 {
	if arity > 127 {
		panic("can not have more than 127 positional parameters")
	}

	positionalArgs := uint32(arity) // First 7 bits: required args
	const variadicBit = 128         // 8-th bit: "rest" arg
	totalArgs := uint32(arity << 8) // Other bits

	if variadic {
		return positionalArgs + variadicBit + totalArgs
	}
	return positionalArgs + totalArgs
}

// Return extended function documentation string.
func docString(f *tu.Func) string {
	if len(f.Params) == 0 {
		return f.DocString
	}
	params := append(make([]string, 0, len(f.Params)), f.Params...)
	if !f.Variadic {
		params[len(params)-1] = "&rest " + params[len(params)-1]
	}
	return f.DocString + "\n(fn " + strings.Join(params, " ") + ")"
}
