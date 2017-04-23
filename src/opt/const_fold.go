package opt

import (
	"emacs/sexp"
)

// Merge sub-expression of the same type into containing S-expr.
// Note that this transformation is valid only when
// isCommutative(op) is true.
// Works for any variadic operation regardless of type.
//
// (+ (+ x y) z) => (+ x y z)
func flattenIntVariadicOp(op *sexp.Operation) *sexp.Operation {
	args := make([]sexp.Node, 0, len(op.Args))
	for _, arg := range op.Args {
		if varOp, ok := arg.(*sexp.Operation); ok && varOp.OpKind == op.OpKind {
			args = append(args, flattenIntVariadicOp(varOp).Args...)
		} else {
			args = append(args, arg)
		}
	}
	op.Args = args
	return op
}

type intReducer func(int64, int64) int64
type floatReducer func(float64, float64) float64

func intBitOr(x, y int64) int64  { return x | y }
func intBitAnd(x, y int64) int64 { return x & y }
func intBitXor(x, y int64) int64 { return x ^ y }
func intAdd(x, y int64) int64    { return x + y }
func intMul(x, y int64) int64    { return x * y }

func floatAdd(x, y float64) float64 { return x + y }
func floatMul(x, y float64) float64 { return x * y }

func floatCommutativeEval(op *sexp.Operation, reducer floatReducer) sexp.Node {
	op = flattenIntVariadicOp(op)
	newArgs := []sexp.Node{}
	toReduce := []float64{}

	for _, arg := range op.Args {
		arg = ConstFold(arg)

		if floatArg, ok := arg.(sexp.Float); ok {
			toReduce = append(toReduce, floatArg.Val)
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
		return sexp.Float{res}
	}
	op.Args = append(op.Args, sexp.Float{res})
	return op
}

func intCommutativeEval(op *sexp.Operation, reducer intReducer) sexp.Node {
	op = flattenIntVariadicOp(op)
	newArgs := []sexp.Node{}
	toReduce := []int64{}

	for _, arg := range op.Args {
		arg = ConstFold(arg)

		if intArg, ok := arg.(sexp.Int); ok {
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
		return sexp.Int{res}
	}
	op.Args = append(op.Args, sexp.Int{res})
	return op
}

func foldFloatVariadicOp(op *sexp.Operation) sexp.Node {
	// Fold commutative ops:
	switch op.OpKind {
	case sexp.OpAdd:
		return floatCommutativeEval(op, floatAdd)
	case sexp.OpMul:
		return floatCommutativeEval(op, floatMul)
	}

	return op
}

func foldIntVariadicOp(op *sexp.Operation) sexp.Node {
	// Fold commutative ops:
	switch op.OpKind {
	case sexp.OpBitOr:
		return intCommutativeEval(op, intBitOr)
	case sexp.OpBitAnd:
		return intCommutativeEval(op, intBitAnd)
	case sexp.OpBitXor:
		return intCommutativeEval(op, intBitXor)
	case sexp.OpAdd:
		return intCommutativeEval(op, intAdd)
	case sexp.OpMul:
		return intCommutativeEval(op, intMul)
	}

	return op
}
