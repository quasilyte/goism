package sexp

import (
	"exn"
)

type rewriteFunc func(Form) Form

// Rewrite provides convenient way to update AST.
func Rewrite(form Form, f rewriteFunc) Form {
	switch form := form.(type) {
	case *Block:
		return rewriteList(form, form.Forms, f)
	case *FormList:
		return rewriteList(form, form.Forms, f)
	case *ExprStmt:
		return rewrite(form, f, &form.Expr)
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
		return rewriteList(form, form.Vals, f)

	case *SparseArrayLit:
		if form := f(form); form != nil {
			return form
		}
		for i := range form.Vals {
			form.Vals[i] = Rewrite(form.Vals[i], f)
		}

	case *SliceLit:
		return rewriteList(form, form.Vals, f)
	case *ArrayIndex:
		return rewrite(form, f, &form.Array, &form.Index)
	case *ArrayUpdate:
		return rewrite(form, f, &form.Array, &form.Index, &form.Expr)
	case *ArraySlice:
		return rewriteSpan(form, &form.Array, &form.Span, f)
	case *SliceIndex:
		return rewrite(form, f, &form.Slice, &form.Index)
	case *SliceUpdate:
		return rewrite(form, f, &form.Slice, &form.Index, &form.Expr)
	case *SliceSlice:
		return rewriteSpan(form, &form.Slice, &form.Span, f)
	case *Bind:
		return rewrite(form, f, &form.Init)
	case *Rebind:
		return rewrite(form, f, &form.Expr)
	case *VarUpdate:
		return rewrite(form, f, &form.Expr)
	case *TypeAssert:
		return rewrite(form, f, &form.Expr)

	case *If:
		if form := f(form); form != nil {
			return form
		}
		form.Cond = Rewrite(form.Cond, f)
		form.Then = Rewrite(form.Then, f).(*Block)
		if form.Else != nil {
			form.Else = Rewrite(form.Else, f)
		}

	case *Return:
		return rewriteList(form, form.Results, f)

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

	case *LispCall:
		return rewriteList(form, form.Args, f)
	case *Call:
		return rewriteList(form, form.Args, f)
	case *InstrCall:
		return rewriteList(form, form.Args, f)

	case *Let:
		if form := f(form); form != nil {
			return form
		}
		for i, bind := range form.Bindings {
			form.Bindings[i] = Rewrite(bind, f).(*Bind)
		}
		if form.Expr == nil {
			form.Stmt = Rewrite(form.Stmt, f)
		} else {
			form.Expr = Rewrite(form.Expr, f)
		}

	case *StructLit:
		return rewriteList(form, form.Vals, f)
	case *StructIndex:
		return rewrite(form, f, &form.Struct)
	case *StructUpdate:
		return rewrite(form, f, &form.Struct, &form.Expr)
	case *And:
		return rewrite(form, f, &form.X, &form.Y)
	case *Or:
		return rewrite(form, f, &form.X, &form.Y)

	case nil:
		return form

	default:
		panic(exn.Logic("unexpected form: %#v", form))
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

func rewriteList(form Form, forms []Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	for i, form := range forms {
		forms[i] = Rewrite(form, f)
	}
	return form
}

func rewrite(form Form, f rewriteFunc, xs ...*Form) Form {
	if form := f(form); form != nil {
		return form
	}
	for _, x := range xs {
		*x = Rewrite(*x, f)
	}
	return form
}

func rewriteAtom(form Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	return form
}
