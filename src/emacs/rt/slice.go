package rt

import (
	"emacs/lisp"
)

type Slice struct {
	data   lisp.Object
	offset int
	len    int
	cap    int
}

func SliceLen(slice Slice) int { return slice.len }
func SliceCap(slice Slice) int { return slice.cap }

// MakeSlice creates a new slice with cap=len.
// All values initialized to specified zero value.
func MakeSlice(length int, zv lisp.Object) Slice {
	return Slice{
		data: lisp.MakeVector(length, zv),
		len:  length,
		cap:  length,
	}
}

// MakeSliceCap creates a new slice.
// Each value within length bounds is initialized to specified zero value.
func MakeSliceCap(length, capacity int, zv lisp.Object) Slice {
	if length == capacity {
		return MakeSlice(length, zv)
	}
	data := lisp.MakeVector(capacity, lisp.Intern("nil"))
	for i := 0; i < length; i++ {
		lisp.Aset(data, i, zv)
	}
	return Slice{data: data, len: length, cap: capacity}
}

// ArrayToSlice constructs a new slice from given data vector.
// Vector is not copied.
func ArrayToSlice(data lisp.Object) Slice {
	length := lisp.Length(data)
	return Slice{data: data, len: length, cap: length}
}

// SliceGet extract slice value using specified index.
func SliceGet(slice Slice, index int) lisp.Object {
	return lisp.Aref(slice.data, slice.offset+index)
}

// SliceSet sets slice value at specified index.
func SliceSet(slice Slice, index int, val lisp.Object) {
	lisp.Aset(slice.data, index+slice.offset, val)
}

// SlicePush = "append(slice, val)".
func SlicePush(slice Slice, val lisp.Object) Slice {
	pos := slice.len
	if pos == slice.cap {
		// Need to extend slice storage.
		// Create a new vector with 1st element set to "val"
		// then re-set slice data with "oldData+newData".
		newData := lisp.MakeVector(memExtendPush, lisp.Intern("nil"))
		lisp.Aset(newData, 0, val)
		// For slices with offset a sub-vector should
		// be taken to avoid memory leaks.
		if slice.offset == 0 {
			newData = lisp.Vconcat(slice.data, newData)
		} else {
			newData = lisp.Vconcat(
				lisp.Substring(slice.data, slice.offset),
				newData,
			)
		}
		return Slice{
			data:   newData,
			len:    pos + 1,
			cap:    slice.cap + memExtendPush,
			offset: 0,
		}
	}
	// Insert new value directly.
	slice.len = pos + 1
	SliceSet(slice, pos, val)
	return slice
}

func sliceLenBound(slice Slice, index int) {
	if index < 0 || index > slice.len {
		lisp.Error("slice bounds out of range")
	}
}

func sliceCapBound(slice Slice, index int) {
	if index < 0 || index > slice.cap {
		lisp.Error("slice bounds out of range")
	}
}
