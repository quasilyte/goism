package lisp

// This package is special.
// It may not be a good idea to edit it manually.

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

// Eq = Return t if the two args are the same Lisp object.
//
//goism:"Eq"->"eq"
func Eq(obj1 any, obj2 any) bool

// MapConcat = Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
// In between each pair of results, stick in SEPARATOR.  Thus, " " as
// SEPARATOR results in spaces between the values returned by FUNCTION.
// SEQUENCE may be a list, a vector, a bool-vector, or a string.
//
//goism:"MapConcat"->"mapconcat"
func MapConcat(function any, sequence Object, separator string) string

// Not = Return t if OBJECT is nil, and return nil otherwise.
//
//goism:"Not"->"not"
func Not(object any) bool

// Concat = Concatenate all the arguments and make the result a string.
// The result is a string whose elements are the elements of all the arguments.
// Each argument may be a string or a list or vector of characters (integers).
//
//goism:"Concat"->"concat"
func Concat(sequences ...any) string

// ArefString = Return the element of ARRAY at index IDX.
// ARRAY may be a vector, a string, a char-table, a bool-vector,
// or a byte-code object.  IDX starts at 0.
//
//goism:"ArefString"->"aref"
func ArefString(array string, idx int) rune

// Aset = Store into the element of ARRAY at index IDX the value NEWELT.
// Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
// bool-vector.  IDX starts at 0.
//
//goism:"Aset"->"aset"
func Aset(array Object, idx int, newElt any)

// Length = Return the length of vector, list or string SEQUENCE.
// A byte-code function object is also allowed.
// If the string contains multibyte characters, this is not necessarily
// the number of bytes in the string; it is the number of characters.
// To get the number of bytes, use ‘string-bytes’.
//
//goism:"Length"->"length"
func Length(sequence any) int

// StringBytes = Return the number of bytes in STRING.
// If STRING is multibyte, this may be greater than the length of STRING.
//
//goism:"StringBytes"->"string-bytes"
func StringBytes(s string) int

// MinInt = Return smallest of all the arguments (which must be numbers or markers).
// The value is always a number; markers are converted to numbers.
//
//goism:"MinInt"->"min"
func MinInt(xs ...int) int

// IsMultibyteString = Return t if OBJECT is a multibyte string.
// Return nil if OBJECT is either a unibyte string, or not a string.
//
//goism:"IsMultibyteString"->"multibyte-string-p"
func IsMultibyteString(object string) bool

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
func Prin1ToString(object Object) string
