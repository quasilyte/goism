package bytecode

import (
	"bytecode/ir"
	"emacs"
	"fmt"
	"sexp"
)

// #FIXME: either make compiler object reusable,
// or make it private type and expose Compile as a
// free standing function.

// Compiler converts Sexp forms into bytecode objects.
type Compiler struct {
	code      *code
	constPool ConstPool
	stack     stack
}

func NewCompiler() *Compiler {
	return &Compiler{code: newCode()}
}

func (cl *Compiler) CompileFunc(params []string, forms []sexp.Form) *Func {
	for _, param := range params {
		cl.stack.pushVar(param)
	}

	cl.compileStmtList(forms)

	return &Func{
		Object: cl.createObject(),
	}
}

func (cl *Compiler) compileStmt(form sexp.Form) {
	switch form := form.(type) {
	case *sexp.Return:
		cl.compileReturn(form)
	case *sexp.If:
		cl.compileIf(form)
	case *sexp.Block:
		cl.compileBlock(form)

	default:
		panic(fmt.Sprintf("unexpected stmt: %#v\n", form))
	}
}

func (cl *Compiler) compileExpr(form sexp.Form) {
	switch form := form.(type) {
	case *sexp.IntAdd:
		cl.compileOp(ir.OpNumAdd, form.Args)
	case *sexp.IntGt:
		cl.compileOp(ir.OpNumGt, form.Args)
	case *sexp.IntEq:
		cl.compileOp(ir.OpNumEq, form.Args)

	case sexp.Int:
		cl.emitConst(cl.constPool.InsertInt(form.Val))
	case sexp.String:
		cl.emitConst(cl.constPool.InsertString(form.Val))
	case sexp.Var:
		cl.emitVar(form.Name, cl.stack.findVar(form.Name))

	default:
		panic(fmt.Sprintf("unexpected expr: %#v\n", form))
	}
}

func (cl *Compiler) compileOp(op ir.Opcode, args []sexp.Form) {
	if len(args) == 2 {
		cl.compileExprList(args)
		cl.emitBinaryOp(op)
	} else {
		cl.compileCall(opFunctions[op], args)
	}
}

func (cl *Compiler) compileCall(fn emacs.Symbol, args []sexp.Form) {
	cpIndex := cl.constPool.InsertSym(fn)
	cl.emitConst(cpIndex)
	cl.compileExprList(args)
	cl.emitCall(len(args))
}

func (cl *Compiler) compileReturn(form *sexp.Return) {
	if len(form.Results) > 1 {
		panic("unimplemented") // issue#1
	}
	if len(form.Results) != 0 {
		cl.compileExpr(form.Results[0])
	}
	cl.emitReturn()
}

func (cl *Compiler) compileIf(form *sexp.If) {
	cl.compileExpr(form.Test)
	jmpRef := cl.emitJmp(ir.OpJmpNil)
	cl.pushBlock("then")
	cl.compileStmtList(form.Then.Forms)
	cl.pushBlock("else")
	jmpRef.bind()
	if form.Else != nil {
		cl.compileStmt(form.Else)
	}
}

func (cl *Compiler) compileBlock(form *sexp.Block) {
	// #FIXME: compile locals.
	cl.compileStmtList(form.Forms)
}

func (cl *Compiler) compileStmtList(forms []sexp.Form) {
	for _, form := range forms {
		cl.compileStmt(form)
	}
}

func (cl *Compiler) compileExprList(forms []sexp.Form) {
	for _, form := range forms {
		cl.compileExpr(form)
	}
}

func (cl *Compiler) emitJmp(op ir.Opcode) jmpRef {
	if op != ir.OpJmp {
		cl.stack.drop(1)
	}
	return cl.code.pushJmp(op)
}

func (cl *Compiler) emitConst(cpIndex int) {
	cl.code.pushInstr(ir.ConstRef(cpIndex))
	cl.stack.pushConst(cpIndex)
}

func (cl *Compiler) emitVar(name string, stIndex int) {
	cl.code.pushInstr(ir.StackRef(stIndex))
	cl.stack.pushVar(name)
}

func (cl *Compiler) emitCall(argc int) {
	cl.stack.drop(argc + 1)
	cl.code.pushInstr(ir.Call(argc))
	cl.stack.pushTmp()
}

func (cl *Compiler) emitBinaryOp(op ir.Opcode) {
	cl.stack.drop(2)
	cl.code.pushOp(op)
	cl.stack.pushTmp()
}

func (cl *Compiler) emitReturn() {
	cl.stack.drop(1)
	cl.code.pushOp(ir.OpReturn)
}

func (cl *Compiler) pushBlock(name string) {
	cl.code.pushBlock(name)
}

func (cl *Compiler) createObject() Object {
	return Object{
		Blocks:     cl.code.blocks,
		ConstPool:  cl.constPool,
		StackUsage: cl.stack.maxSize,
	}
}

var opFunctions = [...]emacs.Symbol{
	ir.OpNumEq:  "=",
	ir.OpNumGt:  ">",
	ir.OpNumAdd: "+",
}
