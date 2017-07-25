package opt

import (
	"cfg"
	"sexp"
	"vmm"
)

func widthOfList(forms []sexp.Form) int {
	res := 0
	for _, form := range forms {
		res += width(form)
	}
	return res
}

func widthOfSpan(span sexp.Span) int {
	res := 0
	if span.Low != nil {
		res += width(span.Low)
	}
	if span.High != nil {
		res += width(span.High)
	}
	return res
}

func widthOfBindList(binds []*sexp.Bind) int {
	res := 0
	for _, bind := range binds {
		res += width(bind.Init)
	}
	return res
}

func width(form sexp.Form) int {
	if sexp.IsEmptyForm(form) {
		return 0
	}

	switch form := form.(type) {
	case sexp.Bool, sexp.Int, sexp.Float, sexp.Str, sexp.Symbol:
		return 1
	case sexp.Var, sexp.Local:
		return 1

	case *sexp.ArrayLit:
		return widthOfList(form.Vals) + 2
	case *sexp.SparseArrayLit:
		return widthOfList(form.Vals)*3 + 2
	case *sexp.SliceLit:
		return widthOfList(form.Vals) + 4
	case *sexp.StructLit:
		return widthOfList(form.Vals) + 2

	case *sexp.ArrayUpdate:
		return width(form.Array) + width(form.Expr) + width(form.Index) + 1
	case *sexp.SliceUpdate:
		return width(form.Slice) + width(form.Expr) + width(form.Index) + 4
	case *sexp.StructUpdate:
		return width(form.Struct) + width(form.Expr) + 1
	case *sexp.Bind:
		return width(form.Init)
	case *sexp.Rebind:
		return width(form.Expr) + 1
	case *sexp.VarUpdate:
		return width(form.Expr)
	case sexp.FormList:
		return widthOfList(form)
	case sexp.Block:
		return widthOfList(form)
	case *sexp.If:
		return width(form.Cond) + width(form.Then) + width(form.Else) + 2
	case *sexp.Switch:
		return -1 // #REFS: 90
	case *sexp.SwitchTrue:
		return -1 // #REFS: 90
	case *sexp.Return:
		return widthOfList(form.Results) + len(form.Results)
	case *sexp.ExprStmt:
		return width(form.Expr) + 1
	case *sexp.Goto:
		return 2
	case *sexp.Label:
		return 0
	case *sexp.Repeat:
		return -1 // #REFS: 90
	case *sexp.DoTimes:
		return -1 // #REFS: 90
	case *sexp.Loop:
		return -1 // #REFS: 90
	case *sexp.While:
		return -1 // #REFS: 90

	case *sexp.ArrayIndex:
		return width(form.Array) + width(form.Index) + 1
	case *sexp.SliceIndex:
		return width(form.Slice) + width(form.Index) + 4
	case *sexp.StructIndex:
		return width(form.Struct) + 1

	case *sexp.ArraySlice:
		return width(form.Array) + widthOfSpan(form.Span) + 4
	case *sexp.SliceSlice:
		return width(form.Slice) + widthOfSpan(form.Span) + 4

	case *sexp.TypeAssert:
		return width(form.Expr) + 4

	case *sexp.Call:
		return widthOfList(form.Args) + 2
	case *sexp.LispCall:
		if vmm.InstrCallCost(form.Fn.Name) != 0 {
			return widthOfList(form.Args) + 1
		}
		if vmm.FuncIsThrowing(form.Fn.Name) {
			return cfg.InlineBudget + 1 // Exceed the limit; do not want to inline
		}
		return widthOfList(form.Args) + 2
	case *sexp.LambdaCall:
		return widthOfList(form.Body) + widthOfBindList(form.Args) + 2
	case *sexp.DynCall:
		return width(form.Callable) + widthOfList(form.Args) + 2

	case *sexp.Let:
		if form.Expr != nil {
			return width(form.Expr) + widthOfBindList(form.Bindings) + 1
		}
		return width(form.Stmt) + widthOfBindList(form.Bindings) + 2
	case *sexp.TypeCast:
		return 0

	case *sexp.And:
		return width(form.X) + width(form.Y) + 2
	case *sexp.Or:
		return width(form.X) + width(form.Y) + 2
	}
	return 0
}
