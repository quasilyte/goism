package opt

import (
	"emacs/sexp"
)

// ConstFold applies constant folding optimization.
//
// (+ 1 2)         => 3
// (+ (+ 2 2 x) 4) => (+ 8 x)
func ConstFold(node sexp.Node) sexp.Node {
	switch node := node.(type) {
	case *sexp.VariadicOp:
		return foldVariadicOp(node)
	case *sexp.BinaryOp:
		return foldBinaryOp(node)

	default:
		return node
	}
}
