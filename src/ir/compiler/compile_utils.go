package compiler

import (
	"go/ast"
	"ir/instr"
	"lisp"
)

// Helper "compile*" methods.

const (
	_any = false
	_all = true
)

func (cl *Compiler) tryStmt(stmt ast.Stmt, ok bool) {
	if !ok {
		panic(errUnexpectedStmt(cl, stmt))
	}
}

func (cl *Compiler) tryExpr(expr ast.Expr, ok bool) {
	if !ok {
		panic(errUnexpectedExpr(cl, expr))
	}
}

func (cl *Compiler) compileStmtList(nodes []ast.Stmt) {
	for _, node := range nodes {
		cl.compileStmt(node)
	}
}

func (cl *Compiler) compileExprList(nodes []ast.Expr) {
	for _, node := range nodes {
		cl.compileExpr(node)
	}
}

func (cl *Compiler) compileCall(fn string, args ...ast.Expr) {
	emit(cl, instr.ConstRef(cl.cvec.InsertSym(fn)))
	cl.compileExprList(args)
	emit(cl, instr.Call(len(args)))
}

func (cl *Compiler) compileInstr(ins instr.Instr, args ...ast.Expr) {
	cl.compileExprList(args)
	emit(cl, ins)
}

func (cl *Compiler) compileBind(ident *ast.Ident, init ast.Expr) {
	cl.compileExpr(init)
	cl.st.Bind(ident.Name)
}

func (cl *Compiler) compileRebind(ident *ast.Ident, val ast.Expr) {
	cl.compileExpr(val)
	if stIndex := cl.st.Find(ident.Name); stIndex != -1 {
		emit(cl, instr.StackSet(stIndex))
		// "-1" because we popped stask element.
		cl.st.Rebind(stIndex-1, ident.Name)
	} else {
		emit(cl, instr.VarSet(cl.cvec.InsertSym(ident.Name)))
	}
}

func (cl *Compiler) compileMultiBind(spec *ast.ValueSpec, index int) {
	if index == 0 {
		cl.compileBind(spec.Names[0], spec.Values[0])
	} else {
		sym := lisp.RetVars[index]
		emit(cl, instr.VarRef(cl.cvec.InsertSym(sym)))
		cl.st.Bind(spec.Names[index].Name)
	}
}
