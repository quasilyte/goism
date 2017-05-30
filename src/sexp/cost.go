package sexp

import (
	"exn"
	"go/types"
)

// Cost returns a value that approximates computational
// complexity of passed argument.
// Works only for expressions.
func Cost(form Form) int {
	// This function contains a large amount of magic numbers.
	// Many aspects can be improved.

	switch form := form.(type) {
	case Bool, Int, Float, Str, Symbol:
		return 1

	case *ArrayLit:
		return cost(form.Vals) + 1

	case *SparseArrayLit:
		valuesCost := 0
		for _, val := range form.Vals {
			valuesCost += Cost(val)
		}
		return valuesCost*3 + 1

	case *SliceLit:
		return cost(form.Vals) + 10

	case *ArrayIndex:
		return Cost(form.Index) + Cost(form.Array) + 1

	case *ArraySlice:
		return Cost(form.Array) + spanCost(form.Span) + 4

	case *SliceIndex:
		return Cost(form.Index) + Cost(form.Slice)*2 + 6

	case *Subslice:
		return Cost(form.Slice) + spanCost(form.Span) + 6

	case *Substr:
		return Cost(form.Str) + spanCost(form.Span) + 1

	case *BinOp:
		return Cost(form.Args[0]) + Cost(form.Args[1]) + baseOpCost[form.Kind]

	case *UnaryOp:
		if form.Kind == OpArrayCopy {
			return Cost(form.X) + int(form.Type().(*types.Array).Len())/2
		}
		return Cost(form.X) + baseOpCost[form.Kind]

	case *Call:
		// #FIXME: this is not correct way to calculate
		// function call cost. We need to know function
		// implementation in order to approximate cost better.
		const defaultFnComplexity = 5
		return callCost(form.Args) + defaultFnComplexity

	case Var:
		// #FIXME: dynamic scope variables should have higher cost.
		return 1

	case *Let:
		bindCost := Cost(form.Bind.Init) + 2
		if form.Expr == nil {
			return Cost(form.Stmt) + bindCost
		}
		return Cost(form.Expr) + bindCost

	default:
		panic(exn.Logic("can not evaluate cost of %#v", form))
	}
}

const baseCallCost = 3

var baseOpCost = [...]int{
	OpShl:    1,
	OpShr:    1,
	OpBitOr:  1,
	OpBitAnd: 1,
	OpBitXor: 1,
	OpAdd:    1,
	OpSub:    1,
	OpMul:    2,
	OpQuo:    3,
	OpNumEq:  1,
	OpNumNeq: 1,
	OpNumLt:  1,
	OpNumLte: 1,
	OpNumGt:  1,
	OpNumGte: 1,

	OpConcat: 8,
	OpStrEq:  3,
	OpStrNeq: 4,
	OpStrLt:  1,
	OpStrLte: 5,
	OpStrGt:  baseCallCost,
	OpStrGte: 5 + baseCallCost,

	OpNot:     1,
	OpNeg:     1,
	OpAdd1:    1,
	OpSub1:    1,
	OpStrCast: 2,

	OpSliceCap: 3,
	OpSliceLen: 3,
}

func spanCost(span Span) int {
	cost := 0
	if span.Low != nil {
		cost += Cost(span.Low)
	}
	if span.High != nil {
		cost += Cost(span.High)
	}
	return cost
}

// Function invocation cost.
// Does not account called function complexity.
func callCost(args []Form) int {
	return cost(args) + len(args) + baseCallCost
}

func cost(forms []Form) int {
	total := 0
	for _, form := range forms {
		total += Cost(form)
	}
	return total
}
