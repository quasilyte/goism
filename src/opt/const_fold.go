package opt

import (
	"emacs/sexp"
)

const (
	maxInt64 int64 = (1 << 63) - 1
)

func isCommutative(op *sexp.VariadicOp) bool {
	switch op.Type {
	case sexp.OpBitOr, sexp.OpBitAnd, sexp.OpBitXor:
		return true
	case sexp.OpAdd, sexp.OpMul:
		return true
	case sexp.OpEq:
		return true

	default:
		return false
	}
}

// Merge sub-expression of the same type into containing S-expr.
// Note that this transformation is valid only when
// isCommutative(op) is true.
//
// (+ (+ x y) z) => (+ x y z)
func opFlatten(op *sexp.VariadicOp) *sexp.VariadicOp {
	args := make([]sexp.Node, 0, len(op.Args))
	for _, arg := range op.Args {
		if varOp, ok := arg.(*sexp.VariadicOp); ok && varOp.Type == op.Type {
			args = append(args, opFlatten(varOp).Args...)
		} else {
			args = append(args, arg)
		}
	}
	op.Args = args
	return op
}

type intReducer func(int64, int64) int64

func intFold(op *sexp.VariadicOp, initial int64, reducer intReducer) sexp.Node {
	newArgs := []sexp.Node{}
	res := initial
	for _, arg := range op.Args {
		optArg := ConstFold(arg)
		if intArg, ok := optArg.(*sexp.Int); ok {
			res = reducer(res, intArg.Val)
		} else {
			newArgs = append(newArgs, optArg)
		}
	}
	if len(newArgs) == 0 {
		return &sexp.Int{res}
	}
	if res != initial {
		newArgs = append(newArgs, &sexp.Int{res})
	}
	op.Args = newArgs
	return op
}

func foldVariadicOp(op *sexp.VariadicOp) sexp.Node {
	if isCommutative(op) {
		op = opFlatten(op)
	}

	switch op.Type {
	case sexp.OpBitOr:
		return intFold(op, 0, func(x, y int64) int64 { return x | y })
	case sexp.OpBitAnd:
		return intFold(op, maxInt64, func(x, y int64) int64 { return x & y })
	case sexp.OpBitXor:
		return intFold(op, 0, func(x, y int64) int64 { return x ^ y })
	case sexp.OpAdd:
		return intFold(op, 0, func(x, y int64) int64 { return x + y })
	case sexp.OpSub:
		return intFold(op, 0, func(x, y int64) int64 { return x - y })
	case sexp.OpMul:
		return intFold(op, 1, func(x, y int64) int64 { return x * y })
	case sexp.OpDiv:
		return intFold(op, 1, func(x, y int64) int64 { return x / y })

	default:
		return op
	}
}
