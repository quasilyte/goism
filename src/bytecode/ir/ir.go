// Package ir provides a low level intermediate representation
// that is very close to the Emacs Lisp bytecode.
//
// This representation makes it possible to perform
// some optimizations that are impossible or hard
// at the higher level. It is also consumed
// to produce bytecode.
package ir

// Opcode determines instruction kind.
//
//go:generate stringer -type=Opcode
type Opcode int16

// Note that:
// - It is possible that not all of these opcodes are generated.
// - Many names are differ from the bytecode listing.
// - IR is not 100% strict mapping of Emacs Lisp bytecode.
//
// Some opcodes are a combination of multiple
// Emacs Lisp opcodes for convenience.
// For example, there is no "dup", only OpStackRef.
//
// To get precise information about opcodes, look elsewhere.
const (
	/*
		Functions
	*/

	// OpReturn returns from a function.
	OpReturn Opcode = iota
	// OpCall invokes function with specified number of arguments.
	OpCall

	/*
		Constants
	*/

	// OpConstRef fetches const vector element and puts it on a stack.
	OpConstRef

	/*
		Stack ops
	*/

	// OpStackRef duplicates nth stack value.
	//   OpStackRef(0) = dup
	OpStackRef
	// OpStackSet changes nth stack element value.
	OpStackSet
	// OpDrop discards N values from stack.
	OpDrop

	/*
		Global variables
	*/

	// OpVarRef puts global variable value on top of the stack.
	OpVarRef
	// OpVarSet sets global variable value.
	OpVarSet

	/*
		Cons ops
	*/

	// OpSetCar changes cons (pair) first value.
	OpSetCar
	// OpSetCdr changes cons (pair) second value.
	OpSetCdr

	/*
		Array ops
	*/

	OpArrayRef
	OpArraySet

	/*
		String ops
	*/

	OpSubstr
	OpConcat
	OpStringEq
	OpStringLt
	OpToLower
	OpToUpper

	/* Predicates */

	// OpIsCons pushes true for cons pair.
	OpIsCons
	// OpIsString pushes true for string.
	OpIsString
	// OpIsNum pushes true for {float|int}.
	OpIsNum
	// OpIsInt pushes true for int.
	OpIsInt

	/* Number ops */

	OpNumAdd
	OpNumAdd1
	OpNumSub
	OpNumSub1
	OpNumMul
	OpNumDiv
	OpNumEq
	OpNumLt
	OpNumLte
	OpNumGt
	OpNumGte
	// OpNumNeg pushes -x.
	OpNumNeg
	// OpNumMin pushes max(x, y).
	OpNumMax
	// OpNumMin pushes min(x, y).
	OpNumMin

	/* Int ops */

	OpRem

	/* Universal ops */

	OpEq
	OpEqual
	OpNot

	/* Constructors */

	OpMakeList
	OpMakeCons

	/* Branching */

	OpJmp
	OpJmpNil
	OpJmpNotNil
	OpJmpNilElsePop
	OpJmpNotNilElsePop
	OpRelJmp
	OpRelJmpNil
	OpRelJmpNotNil
	OpRelJmpNilElsePop
	OpRelJmpNotNilElsePop

	/* Exceptions */

	OpCatch
)

type Instr struct {
	Op  Opcode
	Arg uint16
}
