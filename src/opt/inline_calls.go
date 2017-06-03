package opt

import (
	"fmt"
	"sexp"
	"tu"
)

func InlineCalls(env *tu.Env, form sexp.Form) sexp.Form {
	switch form := form.(type) {
	case *sexp.Block:
		form.Forms = inlineCalls(env, form.Forms)
	case *sexp.Return:
		form.Results = inlineCalls(env, form.Results)

	case *sexp.Call:
		fn := env.Func(form.Fn.Name())
		body := fn.Body
		if !inlineable(body) {
			return form
		}
		expr := body.Forms[0].(*sexp.Return).Results[0]
		fmt.Printf("expr=%#v\n", expr)
		bindings := make([]*sexp.Bind, len(fn.Params))
		for i, paramName := range fn.Params {
			bindings[i] = &sexp.Bind{
				Name: paramName,
				Init: form.Args[i],
			}
		}
		return &sexp.Let{
			Bindings: bindings,
			Expr:     expr,
		}

	}
	return form
}

func inlineable(body *sexp.Block) bool {
	if len(body.Forms) != 1 {
		return false
	}
	form, ok := body.Forms[0].(*sexp.Return)
	if !ok {
		return false
	}
	return len(form.Results) == 1 && sexp.Cost(form) <= 20
}

func inlineCalls(env *tu.Env, forms []sexp.Form) []sexp.Form {
	for i, form := range forms {
		forms[i] = InlineCalls(env, form)
	}
	return forms
}
