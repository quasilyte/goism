package opt

import (
	"emacs/sexp"
	"go/types"
)

// ConstFold applies constant folding optimization.
//
// (+ 1 2)         => 3
// (+ (+ 2 2 x) 4) => (+ 8 x)
func ConstFold(node sexp.Node) sexp.Node {
	switch node := node.(type) {
	case *sexp.VariadicOp:
		if node.Typ.Kind() == types.Float64 {
			return foldFloatVariadicOp(node)
		} else if node.Typ.Kind() == types.Int64 {
			return foldIntVariadicOp(node)
		}

	case *sexp.BinaryOp:
		if node.Typ.Kind() == types.Int64 {
			return foldIntBinaryOp(node)
		}
	}

	return node
}
