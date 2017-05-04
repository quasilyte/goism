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

	/* Exceptions */

	OpCatch

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
)

type Instr struct {
	Op   Opcode
	Data uint16
}

func Return() Instr              { return instr(OpReturn, 0) }
func Call(argc int) Instr        { return instr(OpCall, argc) }
func ConstRef(cpIndex int) Instr { return instr(OpConstRef, cpIndex) }
func StackRef(stIndex int) Instr { return instr(OpStackRef, stIndex) }
func StackSet(stIndex int) Instr { return instr(OpStackSet, stIndex) }
func Drop(count int) Instr       { return instr(OpDrop, count) }
func VarRef(cpIndex int) Instr   { return instr(OpVarRef, cpIndex) }
func VarSet(cpIndex int) Instr   { return instr(OpVarSet, cpIndex) }
func SetCar() Instr              { return instr(OpSetCar, 0) }
func SetCdr() Instr              { return instr(OpSetCdr, 0) }
func ArrayRef() Instr            { return instr(OpArrayRef, 0) }
func ArraySet() Instr            { return instr(OpArraySet, 0) }
func Substr() Instr              { return instr(OpSubstr, 0) }
func Concat(argc int) Instr      { return instr(OpConcat, argc) }
func StringEq() Instr            { return instr(OpStringEq, 0) }
func StringLt() Instr            { return instr(OpStringLt, 0) }
func ToLower() Instr             { return instr(OpToLower, 0) }
func ToUpper() Instr             { return instr(OpToUpper, 0) }
func IsCons() Instr              { return instr(OpIsCons, 0) }
func IsString() Instr            { return instr(OpIsString, 0) }
func IsNum() Instr               { return instr(OpIsNum, 0) }
func IsInt() Instr               { return instr(OpIsInt, 0) }
func NumAdd() Instr              { return instr(OpNumAdd, 0) }
func NumAdd1() Instr             { return instr(OpNumAdd1, 0) }
func NumSub() Instr              { return instr(OpNumSub, 0) }
func NumSub1() Instr             { return instr(OpNumSub1, 0) }
func NumMul() Instr              { return instr(OpNumMul, 0) }
func NumDiv() Instr              { return instr(OpNumDiv, 0) }
func NumEq() Instr               { return instr(OpNumEq, 0) }
func NumLt() Instr               { return instr(OpNumLt, 0) }
func NumLte() Instr              { return instr(OpNumLte, 0) }
func NumGt() Instr               { return instr(OpNumGt, 0) }
func NumGte() Instr              { return instr(OpNumGte, 0) }
func NumNeg() Instr              { return instr(OpNumNeg, 0) }
func NumMax() Instr              { return instr(OpNumMax, 0) }
func NumMin() Instr              { return instr(OpNumMin, 0) }
func Rem() Instr                 { return instr(OpRem, 0) }
func Eq() Instr                  { return instr(OpEq, 0) }
func Equal() Instr               { return instr(OpEqual, 0) }
func Not() Instr                 { return instr(OpNot, 0) }
func MakeList(argc int) Instr    { return instr(OpMakeList, argc) }
func MakeCons() Instr            { return instr(OpMakeCons, 0) }
func Catch() Instr               { return instr(OpCall, 0) }

func Jmp(bbIndex int) Instr {
	return instr(OpJmp, bbIndex)
}
func JmpNil(bbIndex int) Instr {
	return instr(OpJmpNil, bbIndex)
}
func JmpNotNil(bbIndex int) Instr {
	return instr(OpJmpNotNil, bbIndex)
}
func JmpNilElsePop(bbIndex int) Instr {
	return instr(OpJmpNilElsePop, bbIndex)
}
func JmpNotNilElsePop(bbIndex int) Instr {
	return instr(OpJmpNotNilElsePop, bbIndex)
}
func RelJmp() Instr {
	return instr(OpRelJmp, 0)
}
func RelJmpNil() Instr {
	return instr(OpRelJmpNil, 0)
}
func RelJmpNotNil() Instr {
	return instr(OpRelJmpNotNil, 0)
}
func RelJmpNilElsePop() Instr {
	return instr(OpRelJmpNilElsePop, 0)
}
func RelJmpNotNilElsePop() Instr {
	return instr(OpRelJmpNotNilElsePop, 0)
}

func instr(op Opcode, data int) Instr {
	return Instr{Op: op, Data: uint16(data)}
}
