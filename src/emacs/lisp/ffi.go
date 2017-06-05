package lisp

// This package is special.
// It may not be a good idea to edit it by manually.

// Not = Return t if OBJECT is nil, and return nil otherwise.
//
//goism:"Not"->"not"
func Not(object any) bool

// MakeVector = Return a newly created vector of length LENGTH, with each element being INIT.
// See also the function ‘vector’.
//
//goism:"MakeVector"->"make-vector"
func MakeVector(length int, init any) Object

// Substring = Return a new string whose contents are a substring of STRING.
// The returned string consists of the characters between index FROM
// (inclusive) and index TO (exclusive) of STRING.  FROM and TO are
// zero-indexed: 0 means the first character of STRING.  Negative values
// are counted from the end of STRING.  If TO is nil, the substring runs
// to the end of STRING.
//
// The STRING argument may also be a vector.  In that case, the return
// value is a new vector that contains the elements between index FROM
// (inclusive) and index TO (exclusive) of that vector argument.
//
// With one argument, just copy STRING (with properties, if any).
//
//goism:"Substring"->"substring"
func Substring(s Object, fromAndTo ...int) Object

// Vconcat = Concatenate all the arguments and make the result a vector.
// The result is a vector whose elements are the elements of all the arguments.
// Each argument may be a list, vector or string.
//
//goism:"Vconcat"->"vconcat"
func Vconcat(sequences ...Object) Object

// Concat = Concatenate all the arguments and make the result a string.
// The result is a string whose elements are the elements of all the arguments.
// Each argument may be a string or a list or vector of characters (integers).
//
//goism:"Concat"->"concat"
func Concat(sequences ...any) string

// Length = Return the length of vector, list or string SEQUENCE.
// A byte-code function object is also allowed.
// If the string contains multibyte characters, this is not necessarily
// the number of bytes in the string; it is the number of characters.
// To get the number of bytes, use ‘string-bytes’.
//
//goism:"Length"->"length"
func Length(sequence any) int

// Aref = Return the element of ARRAY at index IDX.
// ARRAY may be a vector, a string, a char-table, a bool-vector,
// or a byte-code object.  IDX starts at 0.
//
//goism:"Aref"->"aref"
func Aref(array Object, idx int) Object

// Aset = Store into the element of ARRAY at index IDX the value NEWELT.
// Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
// bool-vector.  IDX starts at 0.
//
//goism:"Aset"->"aset"
func Aset(array Object, idx int, newElt any)

// Signal = Signal an error.  Args are ERROR-SYMBOL and associated DATA.
// This function does not return.
//
// An error symbol is a symbol with an ‘error-conditions’ property
// that is a list of condition names.
// A handler for any of those names will get to handle this signal.
// The symbol ‘error’ should normally be one of them.
//
// DATA should be a list.  Its elements are printed as part of the error message.
// See Info anchor ‘(elisp)Definition of signal’ for some details on how this
// error message is constructed.
// If the signal is handled, DATA is made available to the handler.
// See also the function ‘condition-case’.
//
//goism:"Signal"->"signal"
func Signal(errorSymbol Symbol, data any)

// Error = Signal an error, making a message by passing args to ‘format-message’.
// In Emacs, the convention is that error messages start with a capital
// letter but *do not* end with a period.  Please follow this convention
// for the sake of consistency.
//
// Note: (error "%s" VALUE) makes the message VALUE without
// interpreting format characters like ‘%’, ‘`’, and ‘'’.
//
//goism:"Error"->"error"
func Error(format string, args ...any)

// IsBool = Return t if OBJECT is one of the two canonical boolean values: t or nil.
// Otherwise, return nil.
//
//goism:"IsBool"->"booleanp"
func IsBool(object Object) bool

// IsInt = Return t if OBJECT is an integer.
//
//goism:"IsInt"->"integerp"
func IsInt(object Object) bool

// IsFloat = Return t if OBJECT is a floating point number.
//
//goism:"IsFloat"->"floatp"
func IsFloat(object Object) bool

// IsString = Return t if OBJECT is a string.
//
//goism:"IsString"->"stringp"
func IsString(object Object) bool

// IsSymbol = Return t if OBJECT is a symbol.
//
//goism:"IsSymbol"->"symbolp"
func IsSymbol(object Object) bool
