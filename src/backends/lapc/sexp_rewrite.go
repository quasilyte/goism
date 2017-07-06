package lapc

import (
	"backends/lapc/ir"
	"exn"
	"go/types"
	"magic_pkg/emacs/lisp"
	"magic_pkg/emacs/rt"
	"opt"
	"sexp"
	"sexpconv"
)

var funcToInstr map[*lisp.Func]ir.Instr

func init() {
	kinds := map[*lisp.Func]ir.InstrKind{
		lisp.FnCons:     ir.Cons,
		lisp.FnCar:      ir.Car,
		lisp.FnCdr:      ir.Cdr,
		lisp.FnAref:     ir.Aref,
		lisp.FnAset:     ir.Aset,
		lisp.FnNumEq:    ir.NumEq,
		lisp.FnNumLt:    ir.NumLt,
		lisp.FnNumGt:    ir.NumGt,
		lisp.FnNumLte:   ir.NumLte,
		lisp.FnNumGte:   ir.NumGte,
		lisp.FnAdd:      ir.Add,
		lisp.FnAdd1:     ir.Add1,
		lisp.FnSub:      ir.Sub,
		lisp.FnSub1:     ir.Sub1,
		lisp.FnMul:      ir.Mul,
		lisp.FnQuo:      ir.Quo,
		lisp.FnMin:      ir.Min,
		lisp.FnStrEq:    ir.StrEq,
		lisp.FnStrLt:    ir.StrLt,
		lisp.FnLen:      ir.Length,
		lisp.FnNot:      ir.Not,
		lisp.FnMemq:     ir.Memq,
		lisp.FnMember:   ir.Member,
		lisp.FnIsInt:    ir.Integerp,
		lisp.FnIsStr:    ir.Stringp,
		lisp.FnIsSymbol: ir.Symbolp,
		lisp.FnEq:       ir.Eq,
		lisp.FnEqual:    ir.Equal,
	}
	funcToInstr = make(map[*lisp.Func]ir.Instr, len(kinds))
	for fn, kind := range kinds {
		funcToInstr[fn] = ir.Instr{Kind: kind}
	}
}

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
		forms[i] = Simplify(form)
	}
	return forms
}

func simplify(form sexp.Form) sexp.Form {
	switch form := form.(type) {
	case *sexp.LispCall:
		args := simplifyList(form.Args)
		switch form.Fn.Sym {
		case "concat":
			return &InstrCall{
				Instr: ir.Instr{Kind: ir.Concat, Data: int32(len(args))},
				Args:  args,
			}
		case "list":
			return &InstrCall{
				Instr: ir.Instr{Kind: ir.List, Data: int32(len(args))},
				Args:  args,
			}
		case "substring":
			for len(args) < 3 {
				args = append(args, sexp.Nil)
			}
			return &InstrCall{
				Instr: ir.Instr{Kind: ir.Substring},
				Args:  args,
			}
		}
		if instr, ok := funcToInstr[form.Fn]; ok {
			return &InstrCall{
				Instr: instr,
				Args:  args,
			}
		}
		form.Args = args
		return form

	case *sexp.SwitchTrue:
		return simplifySwitch(
			form.SwitchBody,
			func(x sexp.Form) sexp.Form { return x },
			0,
		)

	case *sexp.Switch:
		typ := form.Expr.Type()
		tag := sexp.Local{Name: "_it", Typ: typ}
		mkCond := func(rhs sexp.Form) sexp.Form {
			cmp := Simplify(comparatorEq(tag, rhs))
			if cmp == nil {
				panic(exn.NoImpl("can not switch over `%s'", typ))
			}
			return cmp
		}
		expr := Simplify(form.Expr)
		return &sexp.Let{
			Bindings: []*sexp.Bind{&sexp.Bind{Name: "_it", Init: expr}},
			Stmt:     simplifySwitch(form.SwitchBody, mkCond, 0),
		}

	case *sexp.SliceLit:
		return simplifiedCall(
			rt.FnArrayToSlice,
			sexp.NewLispCall(lisp.FnVector, form.Vals...),
		)

	case *sexp.ArrayLit:
		return sexp.NewLispCall(lisp.FnVector, simplifyList(form.Vals)...)

	case *sexp.ArraySlice:
		array := form.Array
		switch form.Kind() {
		case sexp.SpanLowOnly:
			return simplifiedCall(rt.FnArraySliceLow, array, form.Low)
		case sexp.SpanHighOnly:
			return simplifiedCall(rt.FnArraySliceHigh, array, form.High)
		case sexp.SpanBoth:
			return simplifiedCall(rt.FnArraySlice2, array, form.Low, form.High)
		case sexp.SpanWhole:
			return simplifiedCall(rt.FnArrayToSlice, array)
		}

	case *sexp.SliceSlice:
		slice := form.Slice
		switch form.Kind() {
		case sexp.SpanLowOnly:
			return simplifiedCall(rt.FnSliceSliceLow, slice, form.Low)
		case sexp.SpanHighOnly:
			return simplifiedCall(rt.FnSliceSliceHigh, slice, form.High)
		case sexp.SpanBoth:
			return simplifiedCall(rt.FnSliceSlice2, slice, form.Low, form.High)
		case sexp.SpanWhole:
			return form.Slice
		}

	case *sexp.TypeCast:
		return form.Form

	case *sexp.DoTimes:
		form.Body = simplifyList(form.Body)
		form.N = Simplify(form.N)

		init := &sexp.Bind{
			Name: form.Iter.Name,
			Init: sexpconv.ZeroValue(form.Iter.Typ),
		}
		post := &sexp.Rebind{
			Name: form.Iter.Name,
			Expr: sexp.NewAdd1(form.Iter),
		}
		return &sexp.While{
			Init: init,
			Cond: sexp.NewNumLt(form.Iter, form.N),
			Post: post,
			Body: form.Body,
		}
	}

	return nil
}

func simplifiedCall(fn *sexp.Func, args ...sexp.Form) sexp.Form {
	call := sexp.NewCall(fn, args...)
	inlinedCall := opt.TryInline(call)
	return Simplify(inlinedCall)
}

func simplifySwitch(b sexp.SwitchBody, mkCond func(sexp.Form) sexp.Form, i int) sexp.Form {
	if i == len(b.Clauses) {
		b.DefaultBody = simplifyList(b.DefaultBody)
		return b.DefaultBody
	}
	cc := b.Clauses[i]
	cc.Body = simplifyList(cc.Body)
	return &sexp.If{
		Cond: mkCond(Simplify(cc.Expr)),
		Then: cc.Body,
		Else: simplifySwitch(b, mkCond, i+1),
	}
}

// Returns a form which is a equallity comparator for two given forms.
// Returns nil when comparison over {"a", "b"} is undefined (or unimplemented).
func comparatorEq(a, b sexp.Form) sexp.Form {
	switch typ := a.Type(); typ := typ.(type) {
	case *types.Basic:
		if typ.Info()&types.IsNumeric != 0 {
			return sexp.NewNumEq(a, b)
		} else if typ.Kind() == types.String {
			return sexp.NewStrEq(a, b)
		}
		return nil

	case *types.Named:
		// #REFS: 60.
		return nil

	default:
		// Fallback to "eq" comparison.
		// Should work for pointer comparisons.
		return sexp.NewLispCall(lisp.FnEq, a, b)
	}
}
