package sexp

import (
	"cfg"
	"vmm"
)

// CostOfList calls each form Cost method and returns
// accumulated cost sum.
func CostOfList(forms []Form) int {
	total := 0
	for _, form := range forms {
		total += form.Cost()
	}
	return total
}

// CostOf is a convenience wrapper over CostOfList.
func CostOf(forms ...Form) int {
	return CostOfList(forms)
}

// CostOfMap is like CostOfList, but operates on map instead of slice.
func CostOfMap(forms map[int]Form) int {
	total := 0
	for _, form := range forms {
		total += form.Cost()
	}
	return total
}

// costOfCall returns function invocation cost.
// Does not account called function complexity.
func costOfCall(args []Form) int {
	return CostOfList(args) + len(args)/2 + cfg.CostFnCallBase
}

func max2(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func maxClauseCost(b *SwitchBody) int {
	res := 0
	for _, cc := range b.Clauses {
		res = max2(res, cc.Expr.Cost()+cc.Body.Cost()+1)
	}
	res = max2(res, b.DefaultBody.Cost())
	return res
}

func costOfSpan(span Span) int {
	cost := 0
	if span.Low != nil {
		cost += span.Low.Cost()
	}
	if span.High != nil {
		cost += span.High.Cost()
	}
	return cost
}

func costOfBindList(binds []*Bind) int {
	cost := 0
	for _, bind := range binds {
		cost += bind.Init.Cost()
	}
	return cost
}

func (atom Bool) Cost() int   { return 1 }
func (atom Int) Cost() int    { return 1 }
func (atom Float) Cost() int  { return 1 }
func (atom Str) Cost() int    { return 1 }
func (atom Symbol) Cost() int { return 1 }
func (atom Var) Cost() int    { return 1 }
func (atom Local) Cost() int  { return 1 }

func (lit *ArrayLit) Cost() int {
	return CostOfList(lit.Vals) + 2
}
func (lit *SparseArrayLit) Cost() int {
	return CostOfMap(lit.Vals)*3 + 2
}
func (lit *SliceLit) Cost() int {
	// #REFS: 76.
	// "10" -> "rt.FnMakeSlice.Complexity".
	return CostOfList(lit.Vals) + 10
}
func (lit *StructLit) Cost() int {
	// #REFS: 77.
	return CostOfList(lit.Vals) + 2
}

func (form *ArrayUpdate) Cost() int {
	return CostOf(form.Array, form.Expr, form.Index) + 2
}
func (form *SliceUpdate) Cost() int {
	// #REFS: 76.
	// "6" -> "rt.FnSliceSet.Complexity".
	return CostOf(form.Slice, form.Expr, form.Index) + 6
}
func (form *StructUpdate) Cost() int {
	// #REFS: 77.
	return CostOf(form.Struct, form.Expr) + 2
}
func (form *Bind) Cost() int {
	return CostOf(form.Init)
}
func (form *Rebind) Cost() int {
	return CostOf(form.Expr) + 1
}
func (form *VarUpdate) Cost() int {
	return CostOf(form.Expr) + 1
}
func (form *FormList) Cost() int {
	return CostOfList(form.Forms)
}
func (form *Block) Cost() int {
	return CostOfList(form.Forms) + 1
}
func (form *If) Cost() int {
	maxBranchCost := max2(form.Then.Cost(), form.Else.Cost())
	return form.Cond.Cost() + maxBranchCost + 2
}
func (form *Switch) Cost() int {
	return form.Expr.Cost() + maxClauseCost(&form.SwitchBody) + 2
}
func (form *SwitchTrue) Cost() int { return maxClauseCost(&form.SwitchBody) + 2 }
func (form *Return) Cost() int {
	return CostOfList(form.Results) + len(form.Results)
}
func (form *ExprStmt) Cost() int {
	return form.Expr.Cost() + 1
}
func (form *Goto) Cost() int  { return 1 }
func (form *Label) Cost() int { return 0 }

func (form *Repeat) Cost() int {
	return form.Body.Cost() * int(form.N)
}
func (form *DoTimes) Cost() int { return -1 }
func (form *Loop) Cost() int    { return -1 }
func (form *While) Cost() int   { return -1 }

func (form *ArrayIndex) Cost() int {
	return CostOf(form.Array, form.Index) + 1
}
func (form *SliceIndex) Cost() int {
	// #REFS: 76.
	// "4" -> "rt.FnSliceGet.Complexity".
	return CostOf(form.Slice, form.Index) + 4
}
func (form *StructIndex) Cost() int {
	switch vmm.StructReprOf(form.Typ) {
	case vmm.StructUnit:
		return 1 + form.Struct.Cost()
	case vmm.StructCons:
		return 1 + form.Index + form.Struct.Cost()
	case vmm.StructVec:
		return 3 + form.Struct.Cost()
	default:
		return 0
	}
}

func (form *ArraySlice) Cost() int {
	return form.Array.Cost() + costOfSpan(form.Span) + 4
}
func (form *SliceSlice) Cost() int {
	return form.Slice.Cost() + costOfSpan(form.Span) + 6
}

func (form *TypeAssert) Cost() int {
	return form.Expr.Cost() + 4
}

func (call *Call) Cost() int {
	return costOfCall(call.Args)
}
func (call *LispCall) Cost() int {
	if cost := vmm.InstrCallCost(call.Fn.Sym); cost != 0 {
		return cost + CostOfList(call.Args)
	}
	if vmm.FuncIsThrowing(call.Fn.Sym) {
		return costOfCall(call.Args) + cfg.CostThrowFuncPenalty
	}
	return costOfCall(call.Args)
}
func (call *LambdaCall) Cost() int {
	return call.Body.Cost() + costOfBindList(call.Args) + 1
}

func (form *Let) Cost() int {
	return form.Expr.Cost() + costOfBindList(form.Bindings)
}
func (form *TypeCast) Cost() int { return 0 }

func (form *And) Cost() int {
	return CostOf(form.X, form.Y) + 3
}
func (form *Or) Cost() int {
	return CostOf(form.X, form.Y) + 3
}

func (form *emptyForm) Cost() int { return 0 }
