package rt

import (
	"emacs/lisp"
)

// Slice - Go slice.
type Slice struct {
	data   lisp.Object
	offset int
	len    int
	cap    int
}

func SliceLen(slice *Slice) int { return slice.len }
func SliceCap(slice *Slice) int { return slice.cap }

// MakeSlice creates a new slice with cap=len.
// All values initialized to specified zero value.
func MakeSlice(length int, zv lisp.Object) *Slice {
	return &Slice{
		data: lisp.Call("make-vector", length, zv),
		len:  length,
		cap:  length,
	}
}

// MakeSliceCap creates a new slice.
// Each value within length bounds is initialized to specified zero value.
func MakeSliceCap(length, capacity int, zv lisp.Object) *Slice {
	if length == capacity {
		return MakeSlice(length, zv)
	}
	data := lisp.Call("make-vector", capacity, lisp.Intern("nil"))
	for i := 0; i < length; i++ {
		lisp.Aset(data, i, zv)
	}
	return &Slice{data: data, len: length, cap: capacity}
}

// ArrayToSlice constructs a new slice from given data vector.
// Vector is not copied.
func ArrayToSlice(data lisp.Object) *Slice {
	length := lisp.Length(data)
	return &Slice{data: data, len: length, cap: length}
}

// SliceGet extract slice value using specified index.
func SliceGet(slice *Slice, index int) lisp.Object {
	return aref(slice.data, slice.offset+index)
}

// SliceSet sets slice value at specified index.
func SliceSet(slice *Slice, index int, val lisp.Object) {
	lisp.Aset(slice.data, index+slice.offset, val)
}

// SlicePush = "append(slice, val)".
func SlicePush(slice *Slice, val lisp.Object) *Slice {
	pos := slice.len
	if pos == slice.cap {
		// Need to extend slice storage.
		// Create a new vector with 1st element set to "val"
		// then re-set slice data with "oldData+newData".
		newData := lisp.Call("make-vector", memExtendPush, lisp.Intern("nil"))
		lisp.Aset(newData, 0, val)
		// For slices with offset a sub-vector should
		// be taken to avoid memory leaks.
		if slice.offset == 0 {
			newData = vconcat2(slice.data, newData)
		} else {
			newData = vconcat2(
				substringFrom(slice.data, slice.offset),
				newData,
			)
		}
		return &Slice{
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

func sliceLenBound(slice *Slice, index int) {
	if index < 0 || index > slice.len {
		lisp.Error("slice bounds out of range")
	}
}

func sliceCapBound(slice *Slice, index int) {
	if index < 0 || index > slice.cap {
		lisp.Error("slice bounds out of range")
	}
}

// SliceCopyFast is SliceCopy specialization that is appliable if both
// `dst' and `src' have zero offset.
func SliceCopyFast(dst, src *Slice) {
	dstData := dst.data
	srcData := src.data
	count := lisp.MinInt(dst.len, src.len)
	for i := 0; i < count; i++ {
		lisp.Aset(dstData, i, aref(srcData, i))
	}
}

// SliceCopy copies one slice contents to another.
// Up to "min(len(dst), len(src))" elements are copied.
func SliceCopy(dst, src *Slice) {
	if dst.offset == 0 && src.offset == 0 {
		SliceCopyFast(dst, src)
		return
	}
	count := lisp.MinInt(dst.len, src.len)
	for i := 0; i < count; i++ {
		SliceSet(dst, i, SliceGet(src, i))
	}
}

// SliceSlice2 = "slice[low:high]".
func SliceSlice2(slice *Slice, low, high int) *Slice {
	sliceLenBound(slice, low)
	sliceCapBound(slice, high)
	return &Slice{
		data:   slice.data,
		offset: slice.offset + low,
		len:    high - low,
		cap:    slice.cap - low,
	}
}

// SliceSliceLow = "slice[low:]".
func SliceSliceLow(slice *Slice, low int) *Slice {
	sliceLenBound(slice, low)
	return &Slice{
		data:   slice.data,
		offset: slice.offset + low,
		len:    slice.len - low,
		cap:    slice.cap - low,
	}
}

// SliceSliceHigh = "slice[:high]".
func SliceSliceHigh(slice *Slice, high int) *Slice {
	sliceCapBound(slice, high)
	return &Slice{
		data:   slice.data,
		offset: slice.offset,
		len:    high,
		cap:    slice.cap,
	}
}

// ArraySlice2 slices an array: "arr[low:high]".
func ArraySlice2(arr lisp.Object, low, high int) *Slice {
	return &Slice{
		data:   arr,
		offset: low,
		len:    high - low,
		cap:    lisp.Length(arr) - low,
	}
}

// ArraySliceLow slices an array: "arr[low:]".
func ArraySliceLow(arr lisp.Object, low int) *Slice {
	length := lisp.Length(arr)
	return &Slice{
		data:   arr,
		offset: low,
		len:    length - low,
		cap:    length - low,
	}
}

// ArraySliceHigh slices an array: "arr[:high]".
func ArraySliceHigh(arr lisp.Object, high int) *Slice {
	return &Slice{
		data:   arr,
		offset: 0,
		len:    high,
		cap:    lisp.Length(arr),
	}
}
