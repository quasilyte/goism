package compiler

import (
	"lisp"
	"sexp"
)

func sym(val lisp.Symbol) sexp.Symbol {
	return sexp.Symbol{Val: val}
}

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
