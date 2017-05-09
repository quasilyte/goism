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
		Pseudo ops
	*/

	// OpEmpty is a dummy opcode that does not generate any
	// code, but can be used to fill holes during optimizations.
	OpEmpty Opcode = iota

	// OpLocalRef is intermediate form of StackRef.
	OpLocalRef
	// OpLocalSet is intermediate form of StackSet.
	OpLocalSet
	// OpLocalBind associates stack value with local variable.
	OpLocalBind

	// OpScopeExit marks scope exit and holds scope size.
	OpScopeExit

	// OpPanic triggers runtime panic.
	OpPanic

	// OpNoreturnCall compiles to ordinary call, but results are ignored.
	OpNoreturnCall

	/*
		Functions
	*/

	// OpReturn returns from a function.
	OpReturn
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
	// OpCar fetches pair 1-st element.
	OpCar
	// OpCdr fetches pair 2-nd element.
	OpCdr

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
	// OpIsSymbol pushes true for lisp.Symbol
	OpIsSymbol

	/* Number ops */

	OpNumAdd
	OpNumAdd1
	OpNumSub
	OpNumSub1
	OpNumMul
	OpNumQuo
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

// Instructions without data.
var (
	Empty    Instr = instr(OpEmpty, 0)
	Panic    Instr = instr(OpPanic, 0)
	Return   Instr = instr(OpReturn, 0)
	SetCar   Instr = instr(OpSetCar, 0)
	SetCdr   Instr = instr(OpSetCdr, 0)
	Car      Instr = instr(OpCar, 0)
	Cdr      Instr = instr(OpCdr, 0)
	ArrayRef Instr = instr(OpArrayRef, 0)
	ArraySet Instr = instr(OpArraySet, 0)
	Substr   Instr = instr(OpSubstr, 0)
	StringEq Instr = instr(OpStringEq, 0)
	StringLt Instr = instr(OpStringLt, 0)
	ToLower  Instr = instr(OpToLower, 0)
	ToUpper  Instr = instr(OpToUpper, 0)
	IsCons   Instr = instr(OpIsCons, 0)
	IsString Instr = instr(OpIsString, 0)
	IsNum    Instr = instr(OpIsNum, 0)
	IsInt    Instr = instr(OpIsInt, 0)
	IsSymbol Instr = instr(OpIsSymbol, 0)
	NumAdd   Instr = instr(OpNumAdd, 0)
	NumAdd1  Instr = instr(OpNumAdd1, 0)
	NumSub   Instr = instr(OpNumSub, 0)
	NumSub1  Instr = instr(OpNumSub1, 0)
	NumMul   Instr = instr(OpNumMul, 0)
	NumQuo   Instr = instr(OpNumQuo, 0)
	NumEq    Instr = instr(OpNumEq, 0)
	NumLt    Instr = instr(OpNumLt, 0)
	NumLte   Instr = instr(OpNumLte, 0)
	NumGt    Instr = instr(OpNumGt, 0)
	NumGte   Instr = instr(OpNumGte, 0)
	NumNeg   Instr = instr(OpNumNeg, 0)
	NumMax   Instr = instr(OpNumMax, 0)
	NumMin   Instr = instr(OpNumMin, 0)
	Rem      Instr = instr(OpRem, 0)
	Eq       Instr = instr(OpEq, 0)
	Equal    Instr = instr(OpEqual, 0)
	Not      Instr = instr(OpNot, 0)
	MakeCons Instr = instr(OpMakeCons, 0)
	Catch    Instr = instr(OpCall, 0)
)

func LocalRef(id int) Instr       { return instr(OpLocalRef, id) }
func LocalSet(id int) Instr       { return instr(OpLocalSet, id) }
func LocalBind(id int) Instr      { return instr(OpLocalBind, id) }
func ScopeExit(size int) Instr    { return instr(OpScopeExit, size) }
func NoreturnCall(argc int) Instr { return instr(OpNoreturnCall, argc) }
func Call(argc int) Instr         { return instr(OpCall, argc) }
func ConstRef(cpIndex int) Instr  { return instr(OpConstRef, cpIndex) }
func StackRef(stIndex int) Instr  { return instr(OpStackRef, stIndex) }
func StackSet(stIndex int) Instr  { return instr(OpStackSet, stIndex) }
func Drop(count int) Instr        { return instr(OpDrop, count) }
func VarRef(cpIndex int) Instr    { return instr(OpVarRef, cpIndex) }
func VarSet(cpIndex int) Instr    { return instr(OpVarSet, cpIndex) }
func Concat(argc int) Instr       { return instr(OpConcat, argc) }
func MakeList(argc int) Instr     { return instr(OpMakeList, argc) }

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
func RelJmp(relAddr int) Instr {
	return instr(OpRelJmp, relAddr)
}
func RelJmpNil(relAddr int) Instr {
	return instr(OpRelJmpNil, relAddr)
}
func RelJmpNotNil(relAddr int) Instr {
	return instr(OpRelJmpNotNil, relAddr)
}
func RelJmpNilElsePop(relAddr int) Instr {
	return instr(OpRelJmpNilElsePop, relAddr)
}
func RelJmpNotNilElsePop(relAddr int) Instr {
	return instr(OpRelJmpNotNilElsePop, relAddr)
}

func instr(op Opcode, data int) Instr {
	return Instr{Op: op, Data: uint16(data)}
}
