package compiler

import (
	"backends/lapc"
	"backends/lapc/ir"
	"sexp"
	"vmm"
)

func compileBool(cl *Compiler, form sexp.Bool) {
	if bool(form) {
		cl.push().ConstRef(cl.cvec.InsertSym("t"))
	} else {
		cl.push().ConstRef(cl.cvec.InsertSym("nil"))
	}
}

func compileVar(cl *Compiler, form sexp.Var) {
	cl.push().XvarRef(form.Name)
}

func compileLocal(cl *Compiler, form sexp.Local) {
	cl.push().XlocalRef(form.Name)
}

func compileSparseArrayLit(cl *Compiler, form *sexp.SparseArrayLit) {
	compileExpr(cl, form.Ctor)
	cl.push().Xbind("array")
	for i, val := range form.Vals {
		cl.push().XlocalRef("array")
		cl.push().ConstRef(cl.cvec.InsertInt(int64(i)))
		compileExpr(cl, val)
		cl.push().Aset()
	}
}

func compileCall(cl *Compiler, name string, args []sexp.Form) {
	cl.push().ConstRef(cl.cvec.InsertSym(name))
	compileExprList(cl, args)
	cl.push().Call(len(args), name)
}

func compileLambdaCall(cl *Compiler, form *sexp.LambdaCall) {
	retLabel := cl.unit.NewLambdaRetLabel()

	prevRetLabel := cl.innerLambdaRet
	cl.innerLambdaRet = retLabel

	cl.push().XlambdaEnter()
	for _, arg := range form.Args {
		compileBind(cl, arg)
	}
	compileStmtList(cl, form.Body)
	cl.push().Label(retLabel)

	cl.innerLambdaRet = prevRetLabel
}

func compileInstrCall(cl *Compiler, form *lapc.InstrCall) {
	compileExprList(cl, form.Args)
	cl.pushInstr(form.Instr)
}

func compileArrayIndex(cl *Compiler, form *sexp.ArrayIndex) {
	compileExpr(cl, form.Array)
	compileExpr(cl, form.Index)
	cl.push().Aref()
}

func compileLetExpr(cl *Compiler, form *sexp.Let) {
	for _, bind := range form.Bindings {
		compileBind(cl, bind)
	}
	compileExpr(cl, form.Expr)
	cl.push().StackSet(len(form.Bindings))
	if len(form.Bindings) > 1 {
		cl.push().Discard(len(form.Bindings) - 1)
	}
}

func compileStructLit(cl *Compiler, form *sexp.StructLit) {
	switch vmm.StructReprOf(form.Typ) {
	case vmm.StructUnit:
		compileExpr(cl, form.Vals[0])
		cl.push().List(1)

	case vmm.StructCons:
		compileExprList(cl, form.Vals)
		cl.pushN(ir.Instr{Kind: ir.Cons}, form.Typ.NumFields()-1)

	case vmm.StructVec:
		call(cl, "vector", form.Vals...)
	}
}

func compileStructIndex(cl *Compiler, form *sexp.StructIndex) {
	switch vmm.StructReprOf(form.Typ) {
	case vmm.StructUnit:
		compileExpr(cl, form.Struct)
		cl.push().Car()

	case vmm.StructCons:
		compileExpr(cl, form.Struct)
		cl.pushN(ir.Instr{Kind: ir.Cdr}, form.Index)
		if form.Typ.NumFields() != form.Index+1 { // Not last index.
			cl.push().Car()
		}

	case vmm.StructVec:
		compileArrayIndex(cl, &sexp.ArrayIndex{
			Array: form.Struct,
			Index: sexp.Int(form.Index),
		})
	}
}

func compileAnd(cl *Compiler, form *sexp.And) {
	resLabel := cl.unit.NewLabel("and")
	compileExpr(cl, form.X)
	cl.push().JmpNilElsePop(resLabel)
	compileExpr(cl, form.Y)
	cl.push().Label(resLabel)
}

func compileOr(cl *Compiler, form *sexp.Or) {
	resLabel := cl.unit.NewLabel("or")
	compileExpr(cl, form.X)
	cl.push().JmpNotNilElsePop(resLabel)
	compileExpr(cl, form.Y)
	cl.push().Label(resLabel)
}
