package sexpconv

import (
	"sexp"
)

// Simplify translates semantic-rich forms into
// more generic and fundamental forms.
// For example, it converts DoTimes to While.
//
// It is necessary to call Simplify before compiling
// forms into IR because compiler does not recognize
// some high level constructs.
func Simplify(form sexp.Form) sexp.Form {
	return sexp.Rewrite(form, simplify)
}

func simplify(form sexp.Form) sexp.Form {
	switch form := form.(type) {
	case *sexp.Shr:
		return &sexp.Shl{
			Arg: form.Arg,
			N:   &sexp.Neg{Arg: form.N},
		}

	case *sexp.DoTimes:
		bindKey := &sexp.Bind{
			Name: form.Iter.Name,
			Init: ZeroValue(form.Iter.Typ),
		}
		form.Body.Forms = append(form.Body.Forms, &sexp.Rebind{
			Name: form.Iter.Name,
			Expr: _addX(form.Iter, 1),
		})
		loop := &sexp.While{
			Cond: _numLt(form.Iter, form.N),
			Body: form.Body,
		}
		return &sexp.Block{
			Forms: []sexp.Form{bindKey, loop},
			Scope: form.Scope,
		}
	}

	return nil
}
