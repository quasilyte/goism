package sexp

import (
	"exn"
	"lang"
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
		return cost(form.Vals) + 2

	case *SparseArrayLit:
		valuesCost := 0
		for _, val := range form.Vals {
			valuesCost += Cost(val)
		}
		return valuesCost*3 + 2

	case *SliceLit:
		return cost(form.Vals) + 10

	case *StructLit:
		return cost(form.Vals) + 2

	case *ArrayIndex:
		return Cost(form.Index) + Cost(form.Array) + 1

	case *ArraySlice:
		return Cost(form.Array) + spanCost(form.Span) + 4

	case *SliceIndex:
		return Cost(form.Index) + Cost(form.Slice)*2 + 6

	case *SliceSlice:
		return Cost(form.Slice) + spanCost(form.Span) + 6

	case *Call:
		// #FIXME: this is not correct way to calculate
		// function call cost. We need to know function
		// implementation in order to approximate cost better.
		const defaultFnComplexity = 5
		return callCost(form.Args) + defaultFnComplexity

	case *LispCall:
		return lispCallCost(form)

	case *InstrCall:
		return cost(form.Args) + 1

	case *StructIndex:
		return structIndexCost(form)

	case Var:
		// #FIXME: dynamic scope variables should have higher cost.
		return 1

	case *Let:
		bindCost := len(form.Bindings) * 2
		for _, bind := range form.Bindings {
			bindCost += Cost(bind.Init)
		}
		return Cost(form.Expr) + bindCost

	case *TypeCast:
		return Cost(form.Form)

	case *And:
		return Cost(form.X) + Cost(form.Y) + 3

	case *Or:
		return Cost(form.X) + Cost(form.Y) + 3

	case *emptyForm:
		return 0

	default:
		panic(exn.Logic("can not evaluate cost of %#v", form))
	}
}

const baseCallCost = 3

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
	return cost(args) + len(args)/2 + baseCallCost
}

func cost(forms []Form) int {
	total := 0
	for _, form := range forms {
		total += Cost(form)
	}
	return total
}

func structIndexCost(form *StructIndex) int {
	switch lang.StructReprOf(form.Typ) {
	case lang.StructUnit:
		return 1 + Cost(form.Struct)
	case lang.StructCons:
		return 1 + form.Index + Cost(form.Struct)
	case lang.StructVec:
		return 3 + Cost(form.Struct)
	default:
		return 0
	}
}

func lispCallCost(form *LispCall) int {
	cost := callCost(form.Args)
	if lang.FuncIsThrowing(form.Fn.Sym) {
		cost += 3
	}
	return cost
}
