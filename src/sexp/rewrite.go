package sexp

import (
	"fmt"
)

type rewriteFunc func(Form) Form

// Rewrite provides convenient way to update AST.
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

	case *Not:
		return rewriteUnaryOp(form, &form.Arg, f)
	case *Neg:
		return rewriteUnaryOp(form, &form.Arg, f)
	case *AddX:
		return rewriteUnaryOp(form, &form.Arg, f)
	case *SubX:
		return rewriteUnaryOp(form, &form.Arg, f)

	case *Shl:
		return rewriteBinOp(form, &form.Args, f)
	case *Shr:
		return rewriteBinOp(form, &form.Args, f)
	case *BitOr:
		return rewriteBinOp(form, &form.Args, f)
	case *BitAnd:
		return rewriteBinOp(form, &form.Args, f)
	case *BitXor:
		return rewriteBinOp(form, &form.Args, f)
	case *Add:
		return rewriteBinOp(form, &form.Args, f)
	case *Sub:
		return rewriteBinOp(form, &form.Args, f)
	case *Mul:
		return rewriteBinOp(form, &form.Args, f)
	case *Quo:
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
	case *StrEq:
		return rewriteBinOp(form, &form.Args, f)
	case *StrNotEq:
		return rewriteBinOp(form, &form.Args, f)
	case *StrLt:
		return rewriteBinOp(form, &form.Args, f)
	case *StrLte:
		return rewriteBinOp(form, &form.Args, f)
	case *StrGt:
		return rewriteBinOp(form, &form.Args, f)
	case *StrGte:
		return rewriteBinOp(form, &form.Args, f)

	case *Concat:
		if form := f(form); form != nil {
			return form
		}
		form.Args = rewriteList(form.Args, f)

	default:
		panic(fmt.Sprintf("unexpected form: %#v", form))
	}

	return form
}

func rewriteList(forms []Form, f rewriteFunc) []Form {
	for i, form := range forms {
		forms[i] = Rewrite(form, f)
	}
	return forms
}

func rewriteUnaryOp(form Form, arg *Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	*arg = Rewrite(*arg, f)
	return form
}

func rewriteBinOp(form Form, args *[2]Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	args[0] = Rewrite(args[0], f)
	args[1] = Rewrite(args[1], f)
	return form
}

func rewriteAtom(form Form, f rewriteFunc) Form {
	if form := f(form); form != nil {
		return form
	}
	return form
}
