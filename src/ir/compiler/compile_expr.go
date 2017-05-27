package compiler

import (
	"go/ast"
	"go/token"
	"go/types"
	"ir/instr"
	"xast"
)

// Compiling expressions.

func (cl *Compiler) compileBasicLit(node *ast.BasicLit) {
	switch val := cl.ast.Literals[node].(type) {
	case int64:
		emit(cl, instr.ConstRef(cl.cvec.InsertInt(val)))
	case float64:
		emit(cl, instr.ConstRef(cl.cvec.InsertFloat(val)))
	case string:
		emit(cl, instr.ConstRef(cl.cvec.InsertString(val)))
	default:
		panic(errUnexpectedExpr(cl, node))
	}
}

func (cl *Compiler) compileIdent(node *ast.Ident) {
	switch node.Name {
	case "true":
		emit(cl, instr.ConstRef(cl.cvec.InsertSym("t")))
	case "false":
		emit(cl, instr.ConstRef(cl.cvec.InsertSym("nil")))

	default:
		if stIndex := cl.st.Find(node.Name); stIndex != -1 {
			emit(cl, instr.StackRef(stIndex))
		} else {
			emit(cl, instr.VarRef(cl.cvec.InsertSym(node.Name)))
		}
		cl.st.Bind(node.Name)
	}
}

func (cl *Compiler) compileCallExpr(node *ast.CallExpr) {
	if fn, ok := node.Fun.(*ast.Ident); ok {
		cl.compileCall(fn.Name, node.Args...)
	} else {
		cl.compileExpr(node.Fun)
		cl.compileExprList(node.Args)
		emit(cl, instr.Call(len(node.Args)))
	}
}

func (cl *Compiler) compileBinaryExpr(node *ast.BinaryExpr) {
	switch kind := cl.ast.OpKinds[node]; kind {
	case xast.OpBitAnd:
		cl.compileCall("logand", node.X, node.Y)
	case xast.OpBitXor:
		cl.compileCall("logxor", node.X, node.Y)
	case xast.OpShl:
		cl.compileCall("lsh", node.X, node.Y)
	default:
		ins := binOpTable[kind]
		if ins.IsInvalid() {
			panic(errCantCompile(cl, node))
		}
		cl.compileInstr(ins, node.X, node.Y)
	}
}

func (cl *Compiler) compileUnaryExpr(node *ast.UnaryExpr) {
	switch node.Op {
	case token.NOT:
		cl.compileInstr(instr.Not, node.X)
	case token.SUB:
		cl.compileInstr(instr.Neg, node.X)
	case token.ADD:
		cl.compileExpr(node.X)

	default:
		panic(errUnexpectedExpr(cl, node))
	}
}

func (cl *Compiler) compileCompositeLit(lit *ast.CompositeLit) {
	switch typ := cl.TypeOf(lit).(type) {
	case *types.Array:
		cl.compileArrayLit(typ, lit)
	default:
		panic(errUnexpectedExpr(cl, lit))
	}
}

func (cl *Compiler) compileArrayLit(typ *types.Array, lit *ast.CompositeLit) {
	if int64(len(lit.Elts)) == typ.Len() {
		cl.compileCall("vector", lit.Elts...)
	} else if len(lit.Elts) == 0 {
		cl.compileCall("make-vector", cl.ast.NewInt(typ.Len()), xast.ZeroValue(typ.Elem()))
	}
}

func (cl *Compiler) compileIndexExpr(expr *ast.IndexExpr) {
	switch cl.TypeOf(expr.X).(type) {
	case *types.Array:
		cl.compileExpr(expr.X)
		cl.compileExpr(expr.Index)
		emit(cl, instr.ArrayRef)
	}
}
