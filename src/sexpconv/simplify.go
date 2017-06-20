package sexpconv

import (
	"exn"
	"magic_pkg/emacs/rt"
	"sexp"
	"sys_info/function"
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

func simplifyList(forms []sexp.Form) []sexp.Form {
	for i, form := range forms {
		forms[i] = sexp.Rewrite(form, simplify)
	}
	return forms
}

func simplify(form sexp.Form) sexp.Form {
	switch form := form.(type) {
	case *sexp.SwitchTrue:
		return simplifySwitch(
			form.SwitchBody,
			func(x sexp.Form) sexp.Form { return x },
			0,
		)

	case *sexp.Switch:
		typ := form.Expr.Type()
		tag := _it(typ)
		mkCond := func(rhs sexp.Form) sexp.Form {
			cmp := comparatorEq(tag, rhs)
			if cmp == nil {
				panic(exn.NoImpl("can not switch over `%s'", typ))
			}
			return cmp
		}
		return _let(form.Expr, simplifySwitch(form.SwitchBody, mkCond, 0))

	case *sexp.SliceLit:
		return sexp.NewCall(
			rt.FnArrayToSlice,
			sexp.NewLispCall(function.Vector, simplifyList(form.Vals)...),
		)

	case *sexp.ArrayLit:
		return sexp.NewLispCall(function.Vector, simplifyList(form.Vals)...)

	case *sexp.ArraySlice:
		switch form.Kind() {
		case sexp.SpanLowOnly:
			return sexp.NewCall(rt.FnArraySliceLow, form.Array, form.Low)
		case sexp.SpanHighOnly:
			return sexp.NewCall(rt.FnArraySliceHigh, form.Array, form.High)
		case sexp.SpanBoth:
			return sexp.NewCall(rt.FnArraySlice2, form.Array, form.Low, form.High)
		case sexp.SpanWhole:
			return sexp.NewCall(rt.FnArrayToSlice, form.Array)
		}

	case *sexp.SliceSlice:
		switch form.Kind() {
		case sexp.SpanLowOnly:
			return sexp.NewCall(rt.FnSliceSliceLow, form.Slice, form.Low)
		case sexp.SpanHighOnly:
			return sexp.NewCall(rt.FnSliceSliceHigh, form.Slice, form.High)
		case sexp.SpanBoth:
			return sexp.NewCall(rt.FnSliceSlice2, form.Slice, form.Low, form.High)
		case sexp.SpanWhole:
			return form.Slice
		}

	case *sexp.TypeCast:
		return form.Form

	case *sexp.DoTimes:
		bindKey := &sexp.Bind{
			Name: form.Iter.Name,
			Init: ZeroValue(form.Iter.Typ),
		}
		form.Body.Forms = append(form.Body.Forms, &sexp.Rebind{
			Name: form.Iter.Name,
			Expr: sexp.NewAdd1(form.Iter),
		})
		loop := &sexp.While{
			Cond: sexp.NewNumLt(form.Iter, form.N),
			Body: form.Body,
		}
		return &sexp.Block{
			Forms: []sexp.Form{bindKey, loop},
		}
	}

	return nil
}

func simplifySwitch(b sexp.SwitchBody, mkCond func(sexp.Form) sexp.Form, i int) sexp.Form {
	if i == len(b.Clauses) {
		return b.DefaultBody
	}
	cc := b.Clauses[i]
	return &sexp.If{
		Cond: mkCond(Simplify(cc.Expr)),
		Then: cc.Body,
		Else: simplifySwitch(b, mkCond, i+1),
	}
}
