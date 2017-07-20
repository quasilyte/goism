package sexp

type rewriteFunc func(Form) Form

// Rewrite provides convenient way to update AST.
func Rewrite(form Form, fn rewriteFunc) Form {
	switch form := form.(type) {
	case Block:
		return rewriteList(form, form, fn)
	case FormList:
		return rewriteList(form, form, fn)
	case *ExprStmt:
		return rewrite(form, fn, &form.Expr)
	case Bool:
		return rewriteAtom(form, fn)
	case Int:
		return rewriteAtom(form, fn)
	case Float:
		return rewriteAtom(form, fn)
	case Str:
		return rewriteAtom(form, fn)
	case Symbol:
		return rewriteAtom(form, fn)
	case Var:
		return rewriteAtom(form, fn)
	case Local:
		return rewriteAtom(form, fn)
	case *ArrayLit:
		return rewriteList(form, form.Vals, fn)

	case *Goto:
		return rewriteAtom(form, fn)
	case *Label:
		return rewriteAtom(form, fn)

	case *SparseArrayLit:
		if form := fn(form); form != nil {
			return form
		}
		for i := range form.Vals {
			form.Vals[i] = Rewrite(form.Vals[i], fn)
		}

	case *SliceLit:
		return rewriteList(form, form.Vals, fn)
	case *ArrayIndex:
		return rewrite(form, fn, &form.Array, &form.Index)
	case *ArrayUpdate:
		return rewrite(form, fn, &form.Array, &form.Index, &form.Expr)
	case *ArraySlice:
		return rewriteSpan(form, &form.Array, &form.Span, fn)
	case *SliceIndex:
		return rewrite(form, fn, &form.Slice, &form.Index)
	case *SliceUpdate:
		return rewrite(form, fn, &form.Slice, &form.Index, &form.Expr)
	case *SliceSlice:
		return rewriteSpan(form, &form.Slice, &form.Span, fn)
	case *Bind:
		return rewrite(form, fn, &form.Init)
	case *Rebind:
		return rewrite(form, fn, &form.Expr)
	case *VarUpdate:
		return rewrite(form, fn, &form.Expr)
	case *TypeAssert:
		return rewrite(form, fn, &form.Expr)

	case *If:
		if form := fn(form); form != nil {
			return form
		}
		form.Cond = Rewrite(form.Cond, fn)
		form.Then = Rewrite(form.Then, fn).(Block)
		form.Else = Rewrite(form.Else, fn)

	case *Switch:
		if form := fn(form); form != nil {
			return form
		}
		form.Expr = Rewrite(form.Expr, fn)
		form.SwitchBody = rewriteSwitchBody(form.SwitchBody, fn)

	case *SwitchTrue:
		if form := fn(form); form != nil {
			return form
		}
		form.SwitchBody = rewriteSwitchBody(form.SwitchBody, fn)

	case *Return:
		return rewriteList(form, form.Results, fn)

	case *Repeat:
		if form := fn(form); form != nil {
			return form
		}
		form.Body = Rewrite(form.Body, fn).(Block)

	case *DoTimes:
		if form := fn(form); form != nil {
			return form
		}
		form.Step = Rewrite(form.Step, fn)
		form.Body = Rewrite(form.Body, fn).(Block)

	case *Loop:
		if form := fn(form); form != nil {
			return form
		}
		form.Body = Rewrite(form.Body, fn).(Block)

	case *While:
		if form := fn(form); form != nil {
			return form
		}
		form.Cond = Rewrite(form.Cond, fn)
		form.Post = Rewrite(form.Post, fn)
		form.Body = Rewrite(form.Body, fn).(Block)

	case *Call:
		return rewriteList(form, form.Args, fn)
	case *LispCall:
		return rewriteList(form, form.Args, fn)
	case *LambdaCall:
		if form := fn(form); form != nil {
			return form
		}
		for i, bind := range form.Args {
			form.Args[i] = Rewrite(bind, fn).(*Bind)
		}
		form.Body = Rewrite(form.Body, fn).(Block)
	case *DynCall:
		if form := fn(form); form != nil {
			return form
		}
		form.Callable = Rewrite(form.Callable, fn)
		for i, arg := range form.Args {
			form.Args[i] = Rewrite(arg, fn)
		}

	case *Let:
		if form := fn(form); form != nil {
			return form
		}
		for i, bind := range form.Bindings {
			form.Bindings[i] = Rewrite(bind, fn).(*Bind)
		}
		if form.Expr == nil {
			form.Stmt = Rewrite(form.Stmt, fn)
		} else {
			form.Expr = Rewrite(form.Expr, fn)
		}

	case *TypeCast:
		return rewrite(form, fn, &form.Form)

	case *StructLit:
		return rewriteList(form, form.Vals, fn)
	case *StructIndex:
		return rewrite(form, fn, &form.Struct)
	case *StructUpdate:
		return rewrite(form, fn, &form.Struct, &form.Expr)
	case *And:
		return rewrite(form, fn, &form.X, &form.Y)
	case *Or:
		return rewrite(form, fn, &form.X, &form.Y)

	case nil:
		return form

	case *emptyForm:
		return form

	default:
		// Can be backend-specific form.
		return rewriteAtom(form, fn)
	}

	return form
}

func rewriteSwitchBody(b SwitchBody, fn rewriteFunc) SwitchBody {
	for i, cc := range b.Clauses {
		b.Clauses[i] = CaseClause{
			Expr: Rewrite(cc.Expr, fn),
			Body: Rewrite(cc.Body, fn).(Block),
		}
	}
	b.DefaultBody = Rewrite(b.DefaultBody, fn).(Block)
	return b
}

func rewriteSpan(form Form, container *Form, span *Span, fn rewriteFunc) Form {
	if form := fn(form); form != nil {
		return form
	}
	*container = Rewrite(*container, fn)
	if span.Low != nil {
		span.Low = Rewrite(span.Low, fn)
	}
	if span.High != nil {
		span.High = Rewrite(span.High, fn)
	}
	return form
}

func rewriteList(form Form, forms []Form, fn rewriteFunc) Form {
	if form := fn(form); form != nil {
		return form
	}
	for i, form := range forms {
		forms[i] = Rewrite(form, fn)
	}
	return form
}

func rewrite(form Form, fn rewriteFunc, xs ...*Form) Form {
	if form := fn(form); form != nil {
		return form
	}
	for _, x := range xs {
		*x = Rewrite(*x, fn)
	}
	return form
}

func rewriteAtom(form Form, fn rewriteFunc) Form {
	if form := fn(form); form != nil {
		return form
	}
	return form
}
