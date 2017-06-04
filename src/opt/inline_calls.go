package opt

import (
	"sexp"
)

// InlineCalls inlines suitable functions calls inside form.
func InlineCalls(form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, func(form sexp.Form) sexp.Form {
		if form, ok := form.(*sexp.Call); ok {
			return inlineCall(form.Fn, form.Args)
		}
		return nil
	})
}

func inlineCall(fn *sexp.Func, args []sexp.Form) sexp.Form {
	body := fn.Body
	expr := inlineableForm(body)
	if expr == nil {
		return nil
	}

	bindings := make([]*sexp.Bind, 0, len(fn.Params))
	for i, param := range fn.Params {
		arg := InlineCalls(args[i])
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

func inlineableForm(body *sexp.Block) sexp.Form {
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
