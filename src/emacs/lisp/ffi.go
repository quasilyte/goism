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

// MapConcat = Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
// In between each pair of results, stick in SEPARATOR.  Thus, " " as
// SEPARATOR results in spaces between the values returned by FUNCTION.
// SEQUENCE may be a list, a vector, a bool-vector, or a string.
//
//goism:"MapConcat"->"mapconcat"
func MapConcat(function any, sequence Object, separator string) Object

// Princ = Output the printed representation of OBJECT, any Lisp object.
// No quoting characters are used; no delimiters are printed around
// the contents of strings.
//
// OBJECT is any of the Lisp data types: a number, a string, a symbol,
// a list, a buffer, a window, a frame, etc.
//
// A printed representation of an object is text which describes that object.
//
// Optional argument PRINTCHARFUN is the output stream, which can be one
// of these:
//
//    - a buffer, in which case output is inserted into that buffer at point;
//    - a marker, in which case output is inserted at marker’s position;
//    - a function, in which case that function is called once for each
//      character of OBJECT’s printed representation;
//    - a symbol, in which case that symbol’s function definition is called; or
//    - t, in which case the output is displayed in the echo area.
//
// If PRINTCHARFUN is omitted, the value of ‘standard-output’ (which see)
// is used instead.
//
//goism:"Princ"->"princ"
func Princ(object any)

// Prin1ToString = Return a string containing the printed representation of OBJECT.
// OBJECT can be any Lisp object.  This function outputs quoting characters
// when necessary to make output that ‘read’ can handle, whenever possible,
// unless the optional second argument NOESCAPE is non-nil.  For complex objects,
// the behavior is controlled by ‘print-level’ and ‘print-length’, which see.
//
// OBJECT is any of the Lisp data types: a number, a string, a symbol,
// a list, a buffer, a window, a frame, etc.
//
// A printed representation of an object is text which describes that object.
//
//goism:"Prin1ToString"->"prin1-to-string"
func Prin1ToString(object any) string

// Eq = Return t if the two args are the same Lisp object.
//
//goism:"Eq"->"eq"
func Eq(obj1 any, obj2 any) bool

// Equal = Return t if two Lisp objects have similar structure and contents.
// They must have the same data type.
// Conses are compared by comparing the cars and the cdrs.
// Vectors and strings are compared element by element.
// Numbers are compared by value, but integers cannot equal floats.
//  (Use ‘=’ if you want integers and floats to be able to be equal.)
// Symbols must match exactly.
//
//goism:"Equal"->"equal"
func Equal(obj1 any, obj2 any) bool

// Puthash = Associate KEY with VALUE in hash table TABLE.
// If KEY is already present in table, replace its current value with
// VALUE.  In any case, return VALUE.
//
//goism:"Puthash"->"puthash"
func Puthash(key any, value any, table Object) Object

// Gethash = Look up KEY in TABLE and return its associated value.
// If KEY is not found, return DFLT which defaults to nil.
//
//goism:"Gethash"->"gethash"
func Gethash(key any, table any, dflt any) Object

// MinInt = Return smallest of all the arguments (which must be numbers or markers).
// The value is always a number; markers are converted to numbers.
//
//goism:"MinInt"->"min"
func MinInt(xs ...int) int

// MinFloat = Return smallest of all the arguments (which must be numbers or markers).
// The value is always a number; markers are converted to numbers.
//
//goism:"MinFloat"->"min"
func MinFloat(xs ...float64) float64

// Message = Display a message at the bottom of the screen.
// The message also goes into the ‘*Messages*’ buffer, if ‘message-log-max’
// is non-nil.  (In keyboard macros, that’s all it does.)
// Return the message.
//
// In batch mode, the message is printed to the standard error stream,
// followed by a newline.
//
// The first argument is a format control string, and the rest are data
// to be formatted under control of the string.  See ‘format-message’ for
// details.
//
// Note: (message "%s" VALUE) displays the string VALUE without
// interpreting format characters like ‘%’, ‘`’, and ‘'’.
//
// If the first argument is nil or the empty string, the function clears
// any existing message; this lets the minibuffer contents show.  See
// also ‘current-message’.
//
//goism:"Message"->"message"
func Message(format string, args ...any) string
