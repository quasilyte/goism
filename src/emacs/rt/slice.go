package rt

/*
(defmacro Go--slice (data offset len cap)
  `(cons ,data
         (cons ,offset
               (cons ,len ,cap))))
(defun Go--make-slice (len zero-val)
  (Go--slice (make-vector len zero-val) 0 len len))
*/

// Need:
// 1) inlining
// 2) structs support

import (
	"emacs/lisp"
)

type slice struct {
	data   lisp.Object
	offset lisp.Int
	len    lisp.Int
	cap    lisp.Int
}

/*
(defun Go--make-slice-from-list (&rest vals)
  (let* ((data (vconcat vals))
         (len (length data)))
    (Go--slice data 0 len len)))
*/

func makeSlice(length lisp.Int, zv lisp.Object) slice {
	return slice{
		data: makeVec(length, zv),
		len:  length,
		cap:  length,
	}
}

func sliceGet(slice slice, index lisp.Int) lisp.Object {
	return lisp.Call("aref", slice.data, index+slice.offset)
}

func sliceSet(slice slice, index lisp.Int, val lisp.Object) {
	lisp.Call("aset", slice.data, index+slice.offset, val)
}

/*
(defun Go--slice-to-str (slice)
  (if (Go--slice-fast? slice)
      (concat (Go--slice-data slice) "")
    (concat (substring (Go--slice-data slice)
                       (Go--slice-offset slice)
                       (+ (Go--slice-offset slice) (Go--slice-len slice)))
            "")))
*/
func bytesToStr(slice slice) lisp.Str {
	if slice.offset == 0 {
		return concat2(slice.data, lisp.Str(""))
	}
	data := copyArraySpan(slice.data, slice.offset, slice.offset+slice.len)
	return concat2(data, lisp.Str(""))
}

/*
func cons4(a, b, c, d lisp.Object) lisp.Object {
	return lisp.Call("cons", a, lisp.Call("cons", b, lisp.Call("cons", c, d)))
}

func makeVector(length lisp.Int, init lisp.Object) lisp.Object {
	return lisp.Call("make-vector", length, init)
}

func makeSlice(length lisp.Int, zv lisp.Object) lisp.Object {
	return cons4(makeVector(length, zv), lisp.Int(0), length, length)
}

func sliceData(slice lisp.Object) lisp.Object {
	return lisp.Call("car", slice)
}

func sliceOffset(slice lisp.Object) lisp.Int {
	return lisp.CallInt("car", lisp.Call("cdr", slice))
}

func sliceGet(slice lisp.Object, index lisp.Int) lisp.Object {
	return lisp.Call("aref", sliceData(slice), sliceOffset(slice)+index)
}
*/
