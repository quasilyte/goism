package bytecode

import (
	"bytecode/ir"
	"emacs"
	"fmt"
	"sexp"
	"tu"
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

func (cl *Compiler) CompileFunc(f *tu.Func) *Func {
	for _, param := range f.Params {
		cl.stack.pushVar(param)
	}

	cl.compileStmtList(f.Body)

	return &Func{
		Object:   cl.createObject(),
		ArgsDesc: argsDescriptor(len(f.Params), f.Variadic),
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
	case *sexp.FormList:
		cl.compileStmtList(form.Forms)
	case *sexp.Bind:
		cl.compileBind(form)
	case *sexp.Rebind:
		cl.compileRebind(form)
	case *sexp.MapSet:
		cl.compileMapSet(form)

	default:
		panic(fmt.Sprintf("unexpected stmt: %#v\n", form))
	}
}

func (cl *Compiler) compileExpr(form sexp.Form) {
	switch form := form.(type) {
	case *sexp.NumAdd:
		cl.compileOp(ir.OpNumAdd, form.Args)
	case *sexp.NumSub:
		cl.compileOp(ir.OpNumSub, form.Args)
	case *sexp.NumMul:
		cl.compileOp(ir.OpNumMul, form.Args)
	case *sexp.NumQuo:
		cl.compileOp(ir.OpNumQuo, form.Args)
	case *sexp.NumGt:
		cl.compileOp(ir.OpNumGt, form.Args)
	case *sexp.NumLt:
		cl.compileOp(ir.OpNumLt, form.Args)
	case *sexp.NumEq:
		cl.compileOp(ir.OpNumEq, form.Args)

	case sexp.Int:
		cl.emitConst(cl.constPool.InsertInt(form.Val))
	case sexp.Float:
		cl.emitConst(cl.constPool.InsertFloat(form.Val))
	case sexp.String:
		cl.emitConst(cl.constPool.InsertString(form.Val))
	case sexp.Var:
		cl.emitVar(form.Name, cl.stack.findVar(form.Name))
	case sexp.Symbol:
		cl.emitConst(cl.constPool.InsertSym(form.Val))

	case *sexp.Call:
		cl.compileCall(emacs.Symbol(form.Fn), form.Args...)

	case sexp.MakeMap:
		cl.compileMakeMap(form)

	default:
		panic(fmt.Sprintf("unexpected expr: %#v\n", form))
	}
}

func (cl *Compiler) compileOp(op ir.Opcode, args []sexp.Form) {
	if len(args) == 2 {
		cl.compileExprList(args)
		cl.emit(ir.Instr{Op: op})
	} else {
		cl.compileCall(instrSpecs[op].fn, args...)
	}
}

func (cl *Compiler) compileCall(fn emacs.Symbol, args ...sexp.Form) {
	cpIndex := cl.constPool.InsertSym(fn)
	cl.emitConst(cpIndex)
	cl.compileExprList(args)
	cl.emitCall(len(args))
}

func (cl *Compiler) compileReturn(form *sexp.Return) {
	if len(form.Results) > 1 {
		panic("unimplemented") // #REFS: 1
	}
	if len(form.Results) != 0 {
		cl.compileExpr(form.Results[0])
	}
	cl.emit(ir.Return())
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
	cl.compileStmtList(form.Forms)
	cl.stack.drop(form.Scope.Len())
}

func (cl *Compiler) compileBind(form *sexp.Bind) {
	cl.compileExpr(form.Init)
	cl.stack.vals[len(cl.stack.vals)-1] = form.Name
}

func (cl *Compiler) compileRebind(form *sexp.Rebind) {
	cl.compileExpr(form.Expr)
	stIndex := cl.stack.findVar(form.Name)
	cl.emit(ir.StackSet(stIndex))
}

func (cl *Compiler) compileMakeMap(form sexp.MakeMap) {
	cl.compileCall(
		"make-hash-table",
		sym(":size"), form.SizeHint,
		sym(":test"), sym("equal"),
	)
}

func (cl *Compiler) compileMapSet(form *sexp.MapSet) {
	cl.compileCall("puthash", form.Key, form.Val, form.Map)
	cl.emit(ir.Drop(1))
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

func (cl *Compiler) emit(instr ir.Instr) {
	spec := instrSpecs[instr.Op]

	cl.stack.drop(spec.argc)
	cl.code.pushInstr(instr)
	if spec.output {
		cl.stack.pushTmp()
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

func sym(val emacs.Symbol) sexp.Symbol {
	return sexp.Symbol{Val: val}
}

func argsDescriptor(arity int, variadic bool) uint32 {
	if arity > 127 {
		panic("can not have more than 127 positional parameters")
	}

	positionalArgs := uint32(arity) // First 7 bits: required args
	const variadicBit = 128         // 8-th bit: "rest" arg
	totalArgs := uint32(arity << 8) // Other bits

	if variadic {
		return positionalArgs + variadicBit + totalArgs
	}
	return positionalArgs + totalArgs
}
