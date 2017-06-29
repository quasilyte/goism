package lapc

import (
	"backends/lapc/instr"
	"exn"
	"go/types"
	"magic_pkg/emacs/lisp"
	"magic_pkg/emacs/rt"
	"sexp"
	"sexpconv"
)

var funcToInstr map[*lisp.Func]instr.Instr

func init() {
	funcToInstr = map[*lisp.Func]instr.Instr{
		lisp.FnCons:     instr.Cons,
		lisp.FnCar:      instr.Car,
		lisp.FnCdr:      instr.Cdr,
		lisp.FnAref:     instr.ArrayRef,
		lisp.FnAset:     instr.ArraySet,
		lisp.FnNumEq:    instr.NumEq,
		lisp.FnNumLt:    instr.NumLt,
		lisp.FnNumGt:    instr.NumGt,
		lisp.FnNumLte:   instr.NumLte,
		lisp.FnNumGte:   instr.NumGte,
		lisp.FnAdd:      instr.NumAdd,
		lisp.FnAdd1:     instr.Add1,
		lisp.FnSub:      instr.NumSub,
		lisp.FnSub1:     instr.Sub1,
		lisp.FnMul:      instr.NumMul,
		lisp.FnQuo:      instr.NumQuo,
		lisp.FnMin:      instr.NumMin,
		lisp.FnStrEq:    instr.StrEq,
		lisp.FnStrLt:    instr.StrLt,
		lisp.FnLen:      instr.Length,
		lisp.FnNot:      instr.Not,
		lisp.FnMemq:     instr.Memq,
		lisp.FnMember:   instr.Member,
		lisp.FnIsInt:    instr.IsInt,
		lisp.FnIsStr:    instr.IsStr,
		lisp.FnIsSymbol: instr.IsSymbol,
		lisp.FnEq:       instr.Eq,
		lisp.FnEqual:    instr.Equal,
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
		args := form.Args
		switch form.Fn.Sym {
		case "concat":
			return &InstrCall{Instr: instr.Concat(len(args)), Args: args}
		case "list":
			return &InstrCall{Instr: instr.List(len(args)), Args: args}
		case "substring":
			for len(args) < 3 {
				args = append(args, sexp.Symbol{Val: "nil"})
			}
			return &InstrCall{Instr: instr.Substr, Args: args}
		}
		if instr, ok := funcToInstr[form.Fn]; ok {
			return &InstrCall{
				Instr: instr,
				Args:  simplifyList(args),
			}
		}
		form.Args = simplifyList(args)
		return form

	case *sexp.SwitchTrue:
		return simplifySwitch(
			form.SwitchBody,
			func(x sexp.Form) sexp.Form { return x },
			0,
		)

	case *sexp.Switch:
		typ := form.Expr.Type()
		tag := sexp.Var{Name: "_it", Typ: typ}
		mkCond := func(rhs sexp.Form) sexp.Form {
			cmp := comparatorEq(tag, rhs)
			if cmp == nil {
				panic(exn.NoImpl("can not switch over `%s'", typ))
			}
			return cmp
		}
		return &sexp.Let{
			Bindings: []*sexp.Bind{&sexp.Bind{Name: "_it", Init: form.Expr}},
			Stmt:     simplifySwitch(form.SwitchBody, mkCond, 0),
		}

	case *sexp.SliceLit:
		return sexp.NewCall(
			rt.FnArrayToSlice,
			sexp.NewLispCall(lisp.FnVector, simplifyList(form.Vals)...),
		)

	case *sexp.ArrayLit:
		return sexp.NewLispCall(lisp.FnVector, simplifyList(form.Vals)...)

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

	case *sexp.Loop:
		return &sexp.While{
			Post: Simplify(form.Post),
			Body: Simplify(form.Body).(*sexp.Block),
		}

	case *sexp.DoTimes:
		bindKey := &sexp.Bind{
			Name: form.Iter.Name,
			Init: sexpconv.ZeroValue(form.Iter.Typ),
		}
		post := &sexp.Rebind{
			Name: form.Iter.Name,
			Expr: sexp.NewAdd1(form.Iter),
		}
		loop := &sexp.While{
			Cond: sexp.NewNumLt(form.Iter, form.N),
			Post: post,
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
