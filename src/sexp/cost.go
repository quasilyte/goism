package sexp

import (
	"fmt"
	"go/types"
)

// Cost returns a value that approximates computational
// complexity of passed argument.
// Works only for expressions.
func Cost(form Form) int {
	// This function contains a large amount of magic numbers.
	// Many aspects can be improved.

	switch form := form.(type) {
	case Bool, Int, Float, String, Symbol:
		return 1

	case *ArrayLit:
		return cost(form.Vals) + 1

	case *SparseArrayLit:
		valuesCost := 0
		for _, val := range form.Vals {
			valuesCost += Cost(val.Expr)
		}
		return valuesCost*3 + 1

	case *SliceLit:
		return cost(form.Vals) + 10

	case *ArrayIndex:
		return Cost(form.Index) + Cost(form.Array) + 1

	case *ArrayCopy:
		return int(form.Type().(*types.Array).Len()) / 2

	case *SliceLen:
		return Cost(form.Slice) + 3

	case *SliceCap:
		return Cost(form.Slice) + 3

	case *SliceIndex:
		return Cost(form.Index) + Cost(form.Slice) + 6

	case *NumAddX:
		return Cost(form.Arg) + int(form.X)
	case *NumSubX:
		return Cost(form.Arg) + int(form.X)
	case *NumAdd:
		return binOpCost(1, form.Args)
	case *NumSub:
		return binOpCost(1, form.Args)
	case *NumMul:
		return binOpCost(2, form.Args)
	case *NumQuo:
		return binOpCost(3, form.Args)
	case *NumEq:
		return binOpCost(1, form.Args)
	case *NumLt:
		return binOpCost(1, form.Args)
	case *NumLte:
		return binOpCost(1, form.Args)
	case *NumGt:
		return binOpCost(1, form.Args)
	case *NumGte:
		return binOpCost(1, form.Args)

	case *Concat:
		return cost(form.Args) + 10
	case *StringEq:
		return binOpCost(2, form.Args)
	case *StringNotEq:
		return binOpCost(3, form.Args)
	case *StringLt:
		return binOpCost(2, form.Args)
	case *StringLte:
		return binOpCost(2, form.Args) + 6
	case *StringGt:
		return callCost(form.Args[:])
	case *StringGte:
		return callCost(form.Args[:]) + 6

	case *Call:
		// #FIXME: this is not correct way to calculate
		// function call cost. We need to know function
		// implementation in order to approximate cost better.
		const defaultFnComplexity = 5
		return callCost(form.Args) + defaultFnComplexity

	case Var:
		// #FIXME: dynamic scope variables should have higher cost.
		return 1

	default:
		panic(fmt.Sprintf("can not evaluate cost of %#v", form))
	}
}

// Function invocation cost.
// Does not account called function complexity.
func callCost(args []Form) int {
	const baseCallCost = 2
	return cost(args) + len(args) + baseCallCost
}

func binOpCost(base int, args [2]Form) int {
	return Cost(args[0]) + Cost(args[1]) + base
}

func cost(forms []Form) int {
	total := 0
	for _, form := range forms {
		total += Cost(form)
	}
	return total
}
