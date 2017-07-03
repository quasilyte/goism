package cfg

// Various options that affect optimizer related behavior.
const (
	// CostFnCallBase - base function call cost.
	CostFnCallBase = 3

	// CostInlineFilter - maximum inline candidate "raw" cost.
	// This filter is applied before all optimizations.
	// Used to reduce number of potentially inlineable functions.
	CostInlineFilter = 28

	// CostInlineThreshold - upper cost limit for inlining candidates.
	// Only functions with body cost less or equal than that threshold
	// are considered "inlineable".
	CostInlineThreshold = 11

	// CostThrowFuncPenalty increases call cost of throwing
	// function by specified amount of points.
	CostThrowFuncPenalty = 4
)
