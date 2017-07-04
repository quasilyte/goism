package opt

import (
	"cfg"
	"sexp"
)

// InlineCalls inlines suitable functions calls inside form.
func InlineCalls(fn *sexp.Func) sexp.Form {
	inl := inliner{fn: fn}
	return inl.rewrite(fn.Body)
}

// TryInline returns inlined function body or the call expression
// itself if inlining is not possible/viable.
func TryInline(form *sexp.Call) sexp.Form {
	inl := inliner{fn: nil}
	return inl.tryInline(form)
}

type inliner struct {
	fn *sexp.Func // Needed to recognize recursive calls
}

func (inl *inliner) rewrite(form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, inl.walkForm)
}

func (inl *inliner) tryInline(form *sexp.Call) sexp.Form {
	if form.Fn == inl.fn {
		// Recursive call. Impossible to inline.
		return form
	}
	if form.Fn.IsInlineable() {
		return inl.inlineCall(form.Fn, form.Args)
	}
	return form
}

func (inl *inliner) walkForm(form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, func(form sexp.Form) sexp.Form {
		if form, ok := form.(*sexp.Call); ok {
			if form.Fn == inl.fn {
				// Recursive call. Impossible to inline.
				return form
			}
			if form.Fn.IsInlineable() {
				return inl.inlineCall(form.Fn, form.Args)
			}
		}
		return nil
	})
}

func (inl *inliner) inlineCall(fn *sexp.Func, args []sexp.Form) sexp.Form {
	// Check if whole function body is single node
	// that can be inlined without LambdaCall.
	if res := inl.inlineAsExpr(fn, args); res != nil {
		return res
	}
	return inl.inlineAsLambdaCall(fn, args)
}

func (inl *inliner) inlineableExpr(fn *sexp.Func) sexp.Form {
	body := fn.Body
	if len(body) == 0 {
		return nil
	}

	worthToInline := func(form sexp.Form) bool {
		return form.Cost() <= cfg.CostInlineThreshold
	}

	switch form := body[0].(type) {
	case *sexp.Return:
		if len(form.Results) != 1 {
			return nil
		}
		if worthToInline(form.Results[0]) {
			return form.Results[0].Copy()
		}

	case *sexp.ExprStmt:
		if !worthToInline(form.Expr) {
			return nil
		}
		// Sole statement is inlineable.
		if len(body) == 1 {
			return form.Expr.Copy()
		}
		// Trailing "return" is permitted (auto-inserted for void functions).
		if len(body) == 2 {
			ret, ok := body[1].(*sexp.Return)
			if ok && len(ret.Results) == 0 {
				return form.Expr.Copy()
			}
		}
	}

	return nil
}

func (inl *inliner) inlineAsExpr(fn *sexp.Func, args []sexp.Form) sexp.Form {
	expr := inl.inlineableExpr(fn)
	if expr == nil {
		return nil
	}

	ctx := inlineCtx{body: expr}
	inl.collectBindings(&ctx, fn.Params, args)

	if len(ctx.bindings) == 0 {
		// Perfect inlining, no let wrapper is needed.
		return ctx.body
	}
	return &sexp.Let{
		Bindings: ctx.bindings,
		Expr:     ctx.body,
	}
}

func (inl *inliner) inlineAsLambdaCall(fn *sexp.Func, args []sexp.Form) sexp.Form {
	if fn.Body.Cost() > cfg.CostInlineThreshold {
		return nil
	}

	ctx := inlineCtx{body: fn.Body.Copy()}
	inl.collectBindings(&ctx, fn.Params, args)

	call := &sexp.LambdaCall{
		Args: ctx.bindings,
		Body: ctx.body.(sexp.Block),
		Typ:  fn.Results,
	}
	return call
}

type inlineCtx struct {
	bindings []*sexp.Bind
	body     sexp.Form
}

func (inl *inliner) collectBindings(ctx *inlineCtx, params []string, args []sexp.Form) {
	ctx.bindings = make([]*sexp.Bind, 0, len(params))
	for i, param := range params {
		arg := inl.rewrite(args[i])

		switch pk := inspectParam(param, ctx.body); {
		case pk == pkUnused:
			// Do nothing.
		case pk == pkUsedOnce || (pk == pkUsedManyTimes && arg.Cost() == 1):
			ctx.body = injectValue(param, arg, ctx.body)
		default:
			ctx.bindings = append(ctx.bindings, &sexp.Bind{
				Name: param,
				Init: arg,
			})
		}
	}
}
