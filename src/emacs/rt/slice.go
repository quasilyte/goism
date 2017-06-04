package rt

import (
	"emacs/lisp"
)

type Slice struct {
	data   lisp.Object
	offset lisp.Int
	len    lisp.Int
	cap    lisp.Int
}

func SliceLen(slice Slice) lisp.Int { return slice.len }
func SliceCap(slice Slice) lisp.Int { return slice.cap }

// MakeSlice creates a new slice with cap=len.
// All values initialized to specified zero value.
func MakeSlice(length lisp.Int, zv lisp.Object) Slice {
	return Slice{
		data: makeVec(length, zv),
		len:  length,
		cap:  length,
	}
}

// MakeSliceCap creates a new slice.
// Each value within length bounds is initialized to specified zero value.
func MakeSliceCap(length, capacity lisp.Int, zv lisp.Object) Slice {
	if length == capacity {
		return MakeSlice(length, zv)
	}
	data := makeVec(capacity, lisp.Intern("nil"))
	for i := lisp.Int(0); i < length; i++ {
		arraySet(data, i, zv)
	}
	return Slice{data: data, len: length, cap: capacity}
}

// ArrayToSlice constructs a new slice from given data vector.
// Vector is not copied.
func ArrayToSlice(data lisp.Object) Slice {
	length := seqLength(data)
	return Slice{data: data, len: length, cap: length}
}

// SliceGet extract slice value using specified index.
func SliceGet(slice Slice, index lisp.Int) lisp.Object {
	return arrayGet(slice.data, slice.offset+index)
}

// SliceSet sets slice value at specified index.
func SliceSet(slice Slice, index lisp.Int, val lisp.Object) {
	arraySet(slice.data, index+slice.offset, val)
}

// BytesToStr converts slice of bytes to string.
func BytesToStr(slice Slice) lisp.Str {
	if slice.offset == 0 {
		return arrayToStr(slice.data)
	}
	return arrayToStr(
		copyArraySpan(slice.data, slice.offset, slice.offset+slice.len),
	)
}

func sliceLenBound(slice Slice, index lisp.Int) {
	if index < 0 || index > slice.len {
		Panic(lisp.Str("slice bounds out of range"))
	}
}

func sliceCapBound(slice Slice, index lisp.Int) {
	if index < 0 || index > slice.cap {
		Panic(lisp.Str("slice bounds out of range"))
	}
}
