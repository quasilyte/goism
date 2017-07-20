package opt

import (
	"magic_pkg/emacs/lisp"
	"sexp"
)

type constexprPass struct {
	triggered bool
}

// FoldConstexpr replaces constexpr with evaluation result.
func FoldConstexpr(fn *sexp.Func) bool {
	p := constexprPass{}
	fn.Body = p.rewrite(fn.Body).(sexp.Block)
	return p.triggered
}

func (p *constexprPass) rewrite(form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, p.walkForm)
}

func (p *constexprPass) walkForm(form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, func(form sexp.Form) sexp.Form {
		if form, ok := form.(*sexp.LispCall); ok {
			return p.foldLispCall(form)
		}
		return nil
	})
}

func (p *constexprPass) foldLispCall(form *sexp.LispCall) sexp.Form {
	switch form.Fn {
	case lisp.FnAdd1:
		if x, ok := form.Args[0].(sexp.Int); ok {
			return sexp.Int(x + 1)
		}
	case lisp.FnSub1:
		if x, ok := form.Args[0].(sexp.Int); ok {
			return sexp.Int(x - 1)
		}
	}
	return nil
}
