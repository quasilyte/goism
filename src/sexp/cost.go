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
	case Bool, Int, Float, Str, Symbol:
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

	case *ArraySlice:
		return Cost(form.Array) + spanCost(form.Span) + 4

	case *SliceLen:
		return Cost(form.Slice) + 3

	case *SliceCap:
		return Cost(form.Slice) + 3

	case *SliceIndex:
		return Cost(form.Index) + Cost(form.Slice)*2 + 6

	case *Subslice:
		return Cost(form.Slice) + spanCost(form.Span) + 6

	case *Substr:
		return Cost(form.Str) + spanCost(form.Span) + 1

	case *AddX:
		return Cost(form.Arg) + int(form.X)
	case *SubX:
		return Cost(form.Arg) + int(form.X)
	case *Add:
		return binOpCost(1, form.Args)
	case *Sub:
		return binOpCost(1, form.Args)
	case *Mul:
		return binOpCost(2, form.Args)
	case *Quo:
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
	case *StrEq:
		return binOpCost(2, form.Args)
	case *StrNotEq:
		return binOpCost(3, form.Args)
	case *StrLt:
		return binOpCost(2, form.Args)
	case *StrLte:
		return binOpCost(2, form.Args) + 6
	case *StrGt:
		return callCost(form.Args[:])
	case *StrGte:
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

	case *Not:
		return Cost(form.Arg) + 1
	case *Neg:
		return Cost(form.Arg) + 1

	case *Let:
		bindCost := Cost(form.Bind.Init) + 2
		if form.Expr == nil {
			return Cost(form.Stmt) + bindCost
		}
		return Cost(form.Expr) + bindCost

	default:
		panic(fmt.Sprintf("can not evaluate cost of %#v", form))
	}
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
