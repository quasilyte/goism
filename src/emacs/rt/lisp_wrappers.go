package rt

import (
	"emacs/lisp"
)

func arrayToStr(arr lisp.Object) string {
	return lisp.Concat(arr, "")
}
