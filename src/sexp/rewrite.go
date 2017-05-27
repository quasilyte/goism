package sexp

import (
	"fmt"
)

type rewriteFunc func(Form) Form

// Rewrite provides convenient way to update AST.
func Rewrite(form Form, f rewriteFunc) Form {
	switch form := form.(type) {
	case *Block:
		form.Forms = rewriteList(form.Forms, f)
	case *FormList:
		form.Forms = rewriteList(form.Forms, f)
	case CallStmt:
		Rewrite(form.Call, f)

	case Bool:
		return rewriteAtom(form, f)
	case Int:
		return rewriteAtom(form, f)
	case Float:
		return rewriteAtom(form, f)
	case Str:
		return rewriteAtom(form, f)
	case Symbol:
		return rewriteAtom(form, f)
	case Var:
		return rewriteAtom(form, f)

	case *ArrayLit:
		if form := f(form); form != nil {
			return form
		}
		form.Vals = rewriteList(form.Vals, f)

	case *SparseArrayLit:
		if form := f(form); form != nil {
			return form
		}
		for i := range form.Vals {
			form.Vals[i].Expr = Rewrite(form.Vals[i].Expr, f)
		}

	case *SliceLit:
		if form := f(form); form != nil {
			return form
		}
		form.Vals = rewriteList(form.Vals, f)

	case *ArrayIndex:
		if form := f(form); form != nil {
			return form
		}
		form.Array = Rewrite(form.Array, f)
		form.Index = Rewrite(form.Index, f)

	case *ArrayUpdate:
		if form := f(form); form != nil {
			return form
		}
		form.Array = Rewrite(form.Array, f)
		form.Index = Rewrite(form.Index, f)
		form.Expr = Rewrite(form.Expr, f)

	case *ArraySlice:
		return rewriteSpan(form, &form.Array, &form.Span, f)

	case *SliceIndex:
		if form := f(form); form != nil {
			return form
		}
		form.Slice = Rewrite(form.Slice, f)
		form.Index = Rewrite(form.Index, f)

	case *SliceUpdate:
		if form := f(form); form != nil {
			return form
		}
		form.Slice = Rewrite(form.Slice, f)
		form.Index = Rewrite(form.Index, f)
		form.Expr = Rewrite(form.Expr, f)

	case *Subslice:
		return rewriteSpan(form, &form.Slice, &form.Span, f)

	case *Substr:
		return rewriteSpan(form, &form.Str, &form.Span, f)

	case *Panic:
		if form := f(form); form != nil {
			return form
		}
		form.ErrorData = Rewrite(form.ErrorData, f)

	case *Bind:
		if form := f(form); form != nil {
			return form
		}
		form.Init = Rewrite(form.Init, f)

	case *Rebind:
		if form := f(form); form != nil {
			return form
		}
		form.Expr = Rewrite(form.Expr, f)

	case *TypeAssert:
		if form := f(form); form != nil {
			return form
		}
		form.Expr = Rewrite(form.Expr, f)

	case *LispTypeAssert:
		if form := f(form); form != nil {
			return form
		}
		form.Expr = Rewrite(form.Expr, f)

	case *If:
		if form := f(form); form != nil {
			return form
		}
		form.Cond = Rewrite(form.Cond, f)
		form.Then = Rewrite(form.Then, f).(*Block)
		form.Else = Rewrite(form.Else, f)

	case *Return:
		if form := f(form); form != nil {
			return form
		}
		form.Results = rewriteList(form.Results, f)

	case *Repeat:
		if form := f(form); form != nil {
			return form
		}
		form.Body = Rewrite(form.Body, f).(*Block)

	case *DoTimes:
		if form := f(form); form != nil {
			return form
		}
		form.Step = Rewrite(form.Step, f)
		form.Body = Rewrite(form.Body, f).(*Block)

	case *While:
		if form := f(form); form != nil {
			return form
		}
		form.Cond = Rewrite(form.Cond, f)
		form.Body = Rewrite(form.Body, f).(*Block)

	case *Call:
		if form := f(form); form != nil {
			return form
		}
		form.Args = rewriteList(form.Args, f)

	case *Let:
		if form := f(form); form != nil {
			return form
		}
		form.Bind = Rewrite(form.Bind, f).(*Bind)
		if form.Expr == nil {
			form.Stmt = Rewrite(form.Stmt, f)
		} else {
			form.Expr = Rewrite(form.Expr, f)
		}

	case *UnaryOp:
		if form := f(form); form != nil {
			return form
		}
		form.X = Rewrite(form.X, f)

	case *BinOp:
		if form := f(form); form != nil {
			return form
		}
		form.Args[0] = Rewrite(form.Args[0], f)
		form.Args[1] = Rewrite(form.Args[1], f)

	default:
		panic(fmt.Sprintf("unexpected form: %#v", form))
	}

	return form
}

func rewriteSpan(form Form, container *Form, span *Span, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	*container = Rewrite(*container, f)
	if span.Low != nil {
		span.Low = Rewrite(span.Low, f)
	}
	if span.High != nil {
		span.High = Rewrite(span.High, f)
	}
	return form
}

func rewriteList(forms []Form, f rewriteFunc) []Form {
	for i, form := range forms {
		forms[i] = Rewrite(form, f)
	}
	return forms
}

func rewriteUnaryOp(form Form, arg *Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	*arg = Rewrite(*arg, f)
	return form
}

func rewriteBinOp(form Form, args *[2]Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	args[0] = Rewrite(args[0], f)
	args[1] = Rewrite(args[1], f)
	return form
}

func rewriteAtom(form Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	return form
}
