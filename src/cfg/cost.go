package cfg

// Options that affect sexp.Cost() return values.
const (
	// CostFnCallBase - base function call cost.
	CostFnCallBase = 3

	// CostThrowFuncPenalty increases call cost of throwing
	// function by specified amount of points.
	CostThrowFuncPenalty = 4
)
