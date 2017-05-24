package sexp

import (
	"fmt"
)

type rewriteFunc func(Form) Form

func rewriteList(forms []Form, f rewriteFunc) []Form {
	for i, form := range forms {
		forms[i] = Rewrite(form, f)
	}
	return forms
}

func rewriteBinOp(form Form, forms *[2]Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	forms[0] = Rewrite(forms[0], f)
	forms[1] = Rewrite(forms[1], f)
	return form
}

func rewriteAtom(form Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	return form
}

func Rewrite(form Form, f rewriteFunc) Form {
	switch form := form.(type) {
	case *Block:
		form.Forms = rewriteList(form.Forms, f)
	case *FormList:
		form.Forms = rewriteList(form.Forms, f)
	case CallStmt:
		Rewrite(form.Call, f)

	case Bool:
		return rewriteAtom(form, f)
	case Int:
		return rewriteAtom(form, f)
	case Float:
		return rewriteAtom(form, f)
	case String:
		return rewriteAtom(form, f)
	case Symbol:
		return rewriteAtom(form, f)
	case Var:
		return rewriteAtom(form, f)

	case *ArrayLit:
		if form := f(form); form != nil {
			return form
		}
		form.Vals = rewriteList(form.Vals, f)

	case *SparseArrayLit:
		if form := f(form); form != nil {
			return form
		}
		for i := range form.Vals {
			form.Vals[i].Expr = Rewrite(form.Vals[i].Expr, f)
		}

	case *SliceLit:
		if form := f(form); form != nil {
			return form
		}
		form.Vals = rewriteList(form.Vals, f)

	case *ArrayIndex:
		if form := f(form); form != nil {
			return form
		}
		form.Array = Rewrite(form.Array, f)
		form.Index = Rewrite(form.Index, f)

	case *ArrayUpdate:
		if form := f(form); form != nil {
			return form
		}
		form.Array = Rewrite(form.Array, f)
		form.Index = Rewrite(form.Index, f)
		form.Expr = Rewrite(form.Expr, f)

	case *ArrayCopy:
		if form := f(form); form != nil {
			return form
		}
		form.Array = Rewrite(form.Array, f)

	case *SliceLen:
		if form := f(form); form != nil {
			return form
		}
		form.Slice = Rewrite(form.Slice, f)

	case *SliceCap:
		if form := f(form); form != nil {
			return form
		}
		form.Slice = Rewrite(form.Slice, f)

	case *SliceIndex:
		if form := f(form); form != nil {
			return form
		}
		form.Slice = Rewrite(form.Slice, f)
		form.Index = Rewrite(form.Index, f)

	case *SliceUpdate:
		if form := f(form); form != nil {
			return form
		}
		form.Slice = Rewrite(form.Slice, f)
		form.Index = Rewrite(form.Index, f)
		form.Expr = Rewrite(form.Expr, f)

	case *Subslice:
		if form := f(form); form != nil {
			return form
		}
		form.Slice = Rewrite(form.Slice, f)
		form.Low = Rewrite(form.Low, f)
		form.High = Rewrite(form.High, f)

	case *Panic:
		if form := f(form); form != nil {
			return form
		}
		form.ErrorData = Rewrite(form.ErrorData, f)

	case *Bind:
		if form := f(form); form != nil {
			return form
		}
		form.Init = Rewrite(form.Init, f)

	case *Rebind:
		if form := f(form); form != nil {
			return form
		}
		form.Expr = Rewrite(form.Expr, f)

	case *TypeAssert:
		if form := f(form); form != nil {
			return form
		}
		form.Expr = Rewrite(form.Expr, f)

	case *LispTypeAssert:
		if form := f(form); form != nil {
			return form
		}
		form.Expr = Rewrite(form.Expr, f)

	case *If:
		if form := f(form); form != nil {
			return form
		}
		form.Cond = Rewrite(form.Cond, f)
		form.Then = Rewrite(form.Then, f).(*Block)
		form.Else = Rewrite(form.Else, f)

	case *Return:
		if form := f(form); form != nil {
			return form
		}
		form.Results = rewriteList(form.Results, f)

	case *Repeat:
		if form := f(form); form != nil {
			return form
		}
		form.Body = Rewrite(form.Body, f).(*Block)

	case *DoTimes:
		if form := f(form); form != nil {
			return form
		}
		form.Step = Rewrite(form.Step, f)
		form.Body = Rewrite(form.Body, f).(*Block)

	case *While:
		if form := f(form); form != nil {
			return form
		}
		form.Cond = Rewrite(form.Cond, f)
		form.Body = Rewrite(form.Body, f).(*Block)

	case *Call:
		if form := f(form); form != nil {
			return form
		}
		form.Args = rewriteList(form.Args, f)

	case *Let:
		if form := f(form); form != nil {
			return form
		}
		form.Bind = Rewrite(form.Bind, f).(*Bind)
		if form.Expr == nil {
			form.Stmt = Rewrite(form.Stmt, f)
		} else {
			form.Expr = Rewrite(form.Expr, f)
		}

	case *Shl:
		if form := f(form); form != nil {
			return form
		}
		form.Arg = Rewrite(form.Arg, f)
		form.N = Rewrite(form.N, f)

	case *Shr:
		if form := f(form); form != nil {
			return form
		}
		form.Arg = Rewrite(form.Arg, f)
		form.N = Rewrite(form.N, f)

	case *Concat:
		if form := f(form); form != nil {
			return form
		}
		form.Args = rewriteList(form.Args, f)

	case *BitOr:
		return rewriteBinOp(form, &form.Args, f)
	case *BitAnd:
		return rewriteBinOp(form, &form.Args, f)
	case *BitXor:
		return rewriteBinOp(form, &form.Args, f)
	case *NumAdd:
		return rewriteBinOp(form, &form.Args, f)
	case *NumSub:
		return rewriteBinOp(form, &form.Args, f)
	case *NumMul:
		return rewriteBinOp(form, &form.Args, f)
	case *NumQuo:
		return rewriteBinOp(form, &form.Args, f)
	case *NumEq:
		return rewriteBinOp(form, &form.Args, f)
	case *NumNotEq:
		return rewriteBinOp(form, &form.Args, f)
	case *NumLt:
		return rewriteBinOp(form, &form.Args, f)
	case *NumLte:
		return rewriteBinOp(form, &form.Args, f)
	case *NumGt:
		return rewriteBinOp(form, &form.Args, f)
	case *NumGte:
		return rewriteBinOp(form, &form.Args, f)
	case *StringEq:
		return rewriteBinOp(form, &form.Args, f)
	case *StringNotEq:
		return rewriteBinOp(form, &form.Args, f)
	case *StringLt:
		return rewriteBinOp(form, &form.Args, f)
	case *StringLte:
		return rewriteBinOp(form, &form.Args, f)
	case *StringGt:
		return rewriteBinOp(form, &form.Args, f)
	case *StringGte:
		return rewriteBinOp(form, &form.Args, f)

	default:
		panic(fmt.Sprintf("unexpected form: %#v", form))
	}

	return form
}
