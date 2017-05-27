package compiler

import (
	"go/ast"
	"ir/instr"
	"lisp"
	"visit"
	"xast"
)

// Compiling statements.

func (cl *Compiler) compileBlockStmt(node *ast.BlockStmt) {
	depth := cl.st.Len()
	cl.compileStmtList(node.List)
	if scopeSize := cl.st.Len() - depth; scopeSize != 0 {
		emit(cl, instr.Discard(scopeSize))
	}
}

func (cl *Compiler) compileReturnStmt(node *ast.ReturnStmt) {
	if len(node.Results) == 0 {
		// Any function in Emacs Lisp must return a value.
		// To avoid Emacs crash, we always return "nil" for void functions.
		emit(cl, instr.ConstRef(cl.cvec.InsertSym("nil")))
		emit(cl, instr.Return)
	} else {
		cl.compileExpr(node.Results[0])
		for i := 1; i < len(node.Results); i++ {
			cl.compileExpr(node.Results[i])
			sym := lisp.RetVars[i]
			emit(cl, instr.VarSet(cl.cvec.InsertSym(sym)))
		}
		emit(cl, instr.Return)
	}
}

func (cl *Compiler) compileIfStmt(node *ast.IfStmt) {
	if node.Else == nil {
		elseLabel := labelCreate(cl, "else")
		cl.compileExpr(node.Cond)
		emitJmpNil(cl, elseLabel)
		cl.compileBlockStmt(node.Body)
		labelBind(cl, elseLabel)
	} else {
		elseLabel := labelCreate(cl, "else")
		endifLabel := labelCreate(cl, "endif")
		cl.compileExpr(node.Cond)
		emitJmpNil(cl, elseLabel)
		cl.compileBlockStmt(node.Body)
		emitJmp(cl, endifLabel)
		labelBind(cl, elseLabel)
		cl.compileStmt(node.Else)
		labelBind(cl, endifLabel)
	}
}

func (cl *Compiler) compileExprStmt(node *ast.ExprStmt) {
	cl.compileExpr(node.X)
	if !xast.IsNoreturnExpr(node.X) {
		emit(cl, instr.Discard(1))
	}
}

func (cl *Compiler) compileDeclStmt(stmt *ast.DeclStmt) {
	decl := stmt.Decl.(*ast.GenDecl)
	cl.tryStmt(stmt, visit.Bind(decl, cl.compileBind))
}

func (cl *Compiler) compileAssignStmt(stmt *ast.AssignStmt) {
	cl.tryStmt(stmt, visit.Rebind(stmt, cl.compileRebind))
}
