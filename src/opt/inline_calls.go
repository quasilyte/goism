package opt

import (
	"sexp"
)

// InlineCalls inlines suitable functions calls inside form.
func InlineCalls(fn *sexp.Func) sexp.Form {
	inl := inliner{fn: fn}
	return inl.rewrite(fn.Body)
}

type inliner struct {
	fn *sexp.Func // Needed to recognize recursive calls
}

func (inl *inliner) rewrite(form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, inl.walkForm)
}

func (inl *inliner) walkForm(form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, func(form sexp.Form) sexp.Form {
		if form, ok := form.(*sexp.Call); ok {
			if form.Fn == inl.fn {
				// Recursive call. Impossible to inline.
				return form
			}
			return inl.inlineCall(form.Fn, form.Args)
		}
		return nil
	})
}

func (inl *inliner) inlineCall(fn *sexp.Func, args []sexp.Form) sexp.Form {
	expr := inl.inlineableForm(fn)
	if expr == nil {
		return nil
	}

	bindings := make([]*sexp.Bind, 0, len(fn.Params))
	for i, param := range fn.Params {
		arg := inl.rewrite(args[i])
		if sexp.Cost(arg) == 1 {
			expr = injectValue(param, arg, expr)
		} else if countUsages(param, expr) == 1 {
			expr = injectValue(param, arg, expr)
		} else {
			bindings = append(bindings, &sexp.Bind{
				Name: param,
				Init: arg,
			})
		}
	}

	if len(bindings) == 0 {
		// Perfect inlining, no let wrapper is needed.
		return expr
	}
	return &sexp.Let{
		Bindings: bindings,
		Expr:     expr,
	}
}

func (inl *inliner) inlineableForm(fn *sexp.Func) sexp.Form {
	body := fn.Body
	if len(body.Forms) == 0 {
		return nil
	}

	worthToInline := func(form sexp.Form) bool {
		return sexp.Cost(form) <= 10
	}

	switch form := body.Forms[0].(type) {
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
		if len(body.Forms) == 1 {
			return form.Expr.Copy()
		}
		// Trailing "return" is permitted (auto-inserted for void functions).
		if len(body.Forms) == 2 {
			ret, ok := body.Forms[1].(*sexp.Return)
			if ok && len(ret.Results) == 0 {
				return form.Expr.Copy()
			}
		}
	}

	return nil
}
