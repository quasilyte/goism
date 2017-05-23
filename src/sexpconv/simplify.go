package sexpconv

import (
	"sexp"
)

func _numLt(a, b sexp.Form) *sexp.NumLt {
	return &sexp.NumLt{Args: [2]sexp.Form{a, b}}
}

func _addX(arg sexp.Form, x int64) *sexp.NumAddX {
	return &sexp.NumAddX{Arg: arg, X: x}
}

// Simplify translates semantic-rich forms into
// more generic and fundamental forms.
// For example, it converts DoTimes to While.
//
// It is necessary to call Simplify before compiling
// forms into IR because compiler does not recognize
// some high level constructs.
func Simplify(form sexp.Form) sexp.Form {
	switch form := form.(type) {
	case *sexp.Block:
		form.Forms = simplify(form.Forms)

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

	return form
}

func simplify(forms []sexp.Form) []sexp.Form {
	for i, form := range forms {
		forms[i] = Simplify(form)
	}
	return forms
}
