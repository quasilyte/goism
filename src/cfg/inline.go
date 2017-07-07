package cfg

// Inliner configuration.
const (
	// InlineFilter - maximum inline candidate "raw" budget.
	// This filter is applied before all optimizations.
	// Used to reduce number of potentially inlineable functions.
	InlineFilter = 28

	// InlineBudget - upper budget limit for inlining candidates.
	InlineBudget = 11
)
