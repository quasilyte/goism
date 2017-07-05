package compiler

import (
	"assert"
	"backends/lapc"
	"backends/lapc/ir"
	"cfg"
	"magic_pkg/emacs/rt"
	"sexp"
	"vmm"
)

func compileBlock(cl *Compiler, form sexp.Block) {
	cl.push().XscopeEnter()
	compileStmtList(cl, form)
	cl.push().XscopeLeave()
}

func compileReturn(cl *Compiler, form *sexp.Return) {
	if cl.innerLambdaRet.Kind == ir.XlambdaRetLabel {
		compileExpr(cl, form.Results[0])
		for i := 1; i < len(form.Results); i++ {
			compileExpr(cl, form.Results[i])
			cl.push().XvarSet(rt.RetVars[i])
		}
		cl.push().XlambdaRet(cl.innerLambdaRet)
		return
	}

	if len(form.Results) == 0 {
		// Any function in Emacs Lisp must return a value.
		// To avoid Emacs crash, we always return "nil" for void functions.
		cl.push().ConstRef(cl.cvec.InsertSym("nil"))
		cl.push().Return()
	} else {
		compileExpr(cl, form.Results[0])
		for i := 1; i < len(form.Results); i++ {
			compileExpr(cl, form.Results[i])
			cl.push().XvarSet(rt.RetVars[i])
		}
		cl.push().Return()
	}
}

func compileIf(cl *Compiler, form *sexp.If) {
	endifLabel := cl.unit.NewLabel("endif")
	if sexp.IsEmptyForm(form.Else) {
		compileExpr(cl, form.Cond)
		cl.push().JmpNil(endifLabel)
		compileBlock(cl, form.Then)
	} else {
		elseLabel := cl.unit.NewLabel("else")
		compileExpr(cl, form.Cond)
		cl.push().JmpNil(elseLabel)
		compileBlock(cl, form.Then)
		cl.push().Jmp(endifLabel)
		cl.push().Label(elseLabel)
		compileStmt(cl, form.Else)
	}
	cl.push().Label(endifLabel)
}

func compileRepeat(cl *Compiler, form *sexp.Repeat) {
	assert.True(form.N <= cfg.ClUnrollHardLimit)
	for i := int64(0); i < form.N; i++ {
		compileBlock(cl, form.Body)
	}
}

func compileLoop(cl *Compiler, form *sexp.Loop) {
	bodyLabel := cl.unit.NewLabel("while-body")
	breakLabel := cl.unit.NewLabel("while-break")
	continueLabel := cl.unit.NewLabel("while-continue")

	prevBreak := cl.innerBreak
	prevContinue := cl.innerContinue
	cl.innerBreak = breakLabel
	cl.innerContinue = continueLabel

	cl.push().XscopeEnter()
	{
		compileStmt(cl, form.Init)
		cl.push().Label(bodyLabel)
		compileBlock(cl, form.Body)
		cl.push().Label(continueLabel)
		compileStmt(cl, form.Post)
		cl.push().Jmp(bodyLabel)
		cl.push().Label(breakLabel)
	}
	cl.push().XscopeLeave()

	cl.innerBreak = prevBreak
	cl.innerContinue = prevContinue
}

func compileWhile(cl *Compiler, form *sexp.While) {
	bodyLabel := cl.unit.NewLabel("while-body")
	breakLabel := cl.unit.NewLabel("while-break")
	continueLabel := cl.unit.NewLabel("while-continue")
	condLabel := cl.unit.NewLabel("while-cond")

	prevBreak := cl.innerBreak
	prevContinue := cl.innerContinue
	cl.innerBreak = breakLabel
	cl.innerContinue = continueLabel

	cl.push().XscopeEnter()
	{
		compileStmt(cl, form.Init)
		cl.push().Jmp(condLabel)
		cl.push().Label(bodyLabel)
		compileBlock(cl, form.Body)
		cl.push().Label(continueLabel)
		compileStmt(cl, form.Post)
		cl.push().Label(condLabel)
		compileExpr(cl, form.Cond)
		cl.push().JmpNotNil(bodyLabel)
		cl.push().Label(breakLabel)
	}
	cl.push().XscopeLeave()

	cl.innerBreak = prevBreak
	cl.innerContinue = prevContinue
}

func compileBind(cl *Compiler, form *sexp.Bind) {
	compileExpr(cl, form.Init)
	cl.push().Xbind(form.Name)
}

func compileRebind(cl *Compiler, form *sexp.Rebind) {
	compileExpr(cl, form.Expr)
	cl.push().XlocalSet(form.Name)
}

func compileVarUpdate(cl *Compiler, form *sexp.VarUpdate) {
	compileExpr(cl, form.Expr)
	cl.push().XvarSet(form.Name)
}

func compileExprStmt(cl *Compiler, form *sexp.ExprStmt) {
	compileExpr(cl, form.Expr)

	if form, ok := form.Expr.(*lapc.InstrCall); ok {
		switch ir.EncodingOf(form.Instr.Kind).Output {
		case ir.AttrPushAndDiscard, ir.AttrPushNothing:
			return // No cleanup is needed
		}
	}

	cl.push().Discard(1)
}

func compileArrayUpdate(cl *Compiler, form *sexp.ArrayUpdate) {
	compileExpr(cl, form.Array)
	compileExpr(cl, form.Index)
	compileExpr(cl, form.Expr)
	cl.push().Aset()
}

func compileStructUpdate(cl *Compiler, form *sexp.StructUpdate) {
	switch vmm.StructReprOf(form.Typ) {
	case vmm.StructUnit:
		compileExpr(cl, form.Struct)
		compileExpr(cl, form.Expr)
		cl.push().SetCar()

	case vmm.StructCons:
		compileExpr(cl, form.Struct)
		if form.Index == 0 {
			// First member.
			compileExpr(cl, form.Expr)
			cl.push().SetCar()
		} else if form.Typ.NumFields() == form.Index+1 {
			// Last member.
			cl.pushN(ir.Instr{Kind: ir.Cdr}, form.Index-1)
			compileExpr(cl, form.Expr)
			cl.push().SetCdr()
		} else {
			cl.pushN(ir.Instr{Kind: ir.Cdr}, form.Index-1)
			compileExpr(cl, form.Expr)
			cl.push().SetCar()
		}

	case vmm.StructVec:
		compileArrayUpdate(cl, &sexp.ArrayUpdate{
			Array: form.Struct,
			Index: sexp.Int(form.Index),
			Expr:  form.Expr,
		})
	}
}

func compileLetStmt(cl *Compiler, form *sexp.Let) {
	for _, bind := range form.Bindings {
		compileBind(cl, bind)
	}
	compileStmt(cl, form.Stmt)
	cl.push().Discard(len(form.Bindings))
}

func compileGoto(cl *Compiler, form *sexp.Goto) {
	switch form.LabelName {
	case "continue":
		cl.push().Xgoto(cl.innerContinue)
	case "break":
		cl.push().Xgoto(cl.innerBreak)
	default:
		cl.push().Xgoto(cl.unit.NewUserLabel(form.LabelName))
	}
}

func compileLabel(cl *Compiler, form *sexp.Label) {
	cl.push().Label(cl.unit.NewUserLabel(form.Name))
}
