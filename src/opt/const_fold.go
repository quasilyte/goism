package opt

import (
	"emacs/sexp"
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

func intEval(op *sexp.BinaryOp, reducer intReducer) sexp.Node {
	op.Arg1 = ConstFold(op.Arg1)
	op.Arg2 = ConstFold(op.Arg2)

	arg1int, ok1 := op.Arg1.(*sexp.Int)
	arg2int, ok2 := op.Arg2.(*sexp.Int)
	if !(ok1 && ok2) {
		return op
	}

	return &sexp.Int{reducer(arg1int.Val, arg2int.Val)}
}

func intFoldEval(op *sexp.VariadicOp, reducer intReducer) sexp.Node {
	newArgs := []sexp.Node{}
	toReduce := []int64{}

	for _, arg := range op.Args {
		arg = ConstFold(arg)

		if intArg, ok := arg.(*sexp.Int); ok {
			toReduce = append(toReduce, intArg.Val)
		} else {
			newArgs = append(newArgs, arg)
		}
	}

	op.Args = newArgs

	if len(toReduce) == 0 {
		return op
	}

	res := toReduce[0]
	for _, x := range toReduce[1:] {
		res = reducer(res, x)
	}

	if len(newArgs) == 0 {
		return &sexp.Int{res}
	}
	op.Args = append(op.Args, &sexp.Int{res})
	return op
}

func foldVariadicOp(op *sexp.VariadicOp) sexp.Node {
	if isCommutative(op) {
		op = opFlatten(op)
	}

	switch op.Type {
	case sexp.OpBitOr:
		return intFoldEval(op, func(x, y int64) int64 { return x | y })
	case sexp.OpBitAnd:
		return intFoldEval(op, func(x, y int64) int64 { return x & y })
	case sexp.OpBitXor:
		return intFoldEval(op, func(x, y int64) int64 { return x ^ y })
	case sexp.OpAdd:
		return intFoldEval(op, func(x, y int64) int64 { return x + y })
	case sexp.OpSub:
		return intFoldEval(op, func(x, y int64) int64 { return x - y })
	case sexp.OpMul:
		return intFoldEval(op, func(x, y int64) int64 { return x * y })
	case sexp.OpDiv:
		return intFoldEval(op, func(x, y int64) int64 { return x / y })

	default:
		return op
	}
}

func foldBinaryOp(op *sexp.BinaryOp) sexp.Node {
	switch op.Type {
	case sexp.OpRem:
		return intEval(op, func(x, y int64) int64 { return x % y })

	default:
		return op
	}
}
