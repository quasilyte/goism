package rt

import (
	"emacs/lisp"
)

// RetN variables store Nth return value of the function.
var (
	Ret2 lisp.Object
	Ret3 lisp.Object
	Ret4 lisp.Object
	Ret5 lisp.Object
	Ret6 lisp.Object
	Ret7 lisp.Object
	Ret8 lisp.Object
)

// MultiRetLimit specifies max N available in RetN variables.
const MultiRetLimit = 8
