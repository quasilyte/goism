package compiler

import (
	"bytecode"
	"bytecode/eval"
	"bytecode/ir"
	"dt"
	"fmt"
	"go/types"
	"lisp"
	"sexp"
	"tu"
)

// These operations have opcode for 2 operands.
// For 2+ operands ordinary function call is used.
var opNames = [...]lisp.Symbol{
	ir.OpNumAdd: "+",
	ir.OpNumSub: "-",
	ir.OpNumMul: "*",
	ir.OpNumQuo: "/",
	ir.OpNumGt:  ">",
	ir.OpNumLt:  "<",
	ir.OpNumEq:  "=",
}

// #FIXME: either make compiler object reusable,
// or make it private type and expose Compile as a
// free standing function.

// Compiler converts Sexp forms into bytecode objects.
type Compiler struct {
	code      *code
	constPool dt.ConstPool
	symPool   dt.SymbolPool
}

func New() *Compiler {
	return &Compiler{
		code: newCode(),
	}
}

func (cl *Compiler) CompileFunc(f *tu.Func) *bytecode.Func {
	for _, param := range f.Params {
		cl.symPool.Insert(param)
	}

	cl.compileStmtList(f.Body)

	object := cl.createObject()
	eval.Object(&object, len(f.Params))
	cl.ensureTrailingReturn()

	return &bytecode.Func{
		Object:   object,
		ArgsDesc: argsDescriptor(len(f.Params), f.Variadic),
	}
}

// Insert trailing "return" opcode if its not already there.
func (cl *Compiler) ensureTrailingReturn() {
	lastInstr := cl.code.lastInstr()

	if lastInstr.Op != ir.OpReturn {
		// Check is needed to avoid generation of "dead" return.
		if lastInstr.Op != ir.OpPanic {
			cl.emit(ir.Return)
		}
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
	case sexp.ExprStmt:
		cl.compileExprStmt(form.Form)
	case *sexp.Panic:
		cl.compilePanic(form.ErrorData)
	case *sexp.While:
		cl.compileWhile(form)

	default:
		panic(fmt.Sprintf("unexpected stmt: %#v\n", form))
	}
}

func (cl *Compiler) compileExpr(form sexp.Form) {
	switch form := form.(type) {
	case *sexp.NumAdd:
		cl.compileAddSub(ir.OpNumAdd, form.Args)
	case *sexp.NumSub:
		cl.compileAddSub(ir.OpNumSub, form.Args)
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
	case *sexp.Concat:
		cl.compileConcat(form)

	case sexp.Int:
		cl.emitConst(cl.constPool.InsertInt(form.Val))
	case sexp.Float:
		cl.emitConst(cl.constPool.InsertFloat(form.Val))
	case sexp.String:
		cl.emitConst(cl.constPool.InsertString(form.Val))
	case sexp.Symbol:
		cl.emitConst(cl.constPool.InsertSym(lisp.Symbol(form.Val)))
	case sexp.Bool:
		cl.compileBool(form)

	case sexp.Var:
		cl.compileVar(form)

	case *sexp.Call:
		cl.compileCall(lisp.Symbol(form.Fn), form.Args...)

	case sexp.MakeMap:
		cl.compileMakeMap(form)

	case *sexp.TypeAssert:
		cl.compileTypeAssert(form)
	case *sexp.LispTypeAssert:
		cl.compileLispTypeAssert(form)

	default:
		panic(fmt.Sprintf("unexpected expr: %#v\n", form))
	}
}

func (cl *Compiler) compileOp(op ir.Opcode, args []sexp.Form) {
	if len(args) == 2 {
		cl.compileExprList(args)
		cl.emit(ir.Instr{Op: op})
	} else {
		cl.compileCall(opNames[op], args...)
	}
}

func (cl *Compiler) compileCall(fn lisp.Symbol, args ...sexp.Form) {
	if !optCall(cl, fn, args) {
		cl.emitConst(cl.constPool.InsertSym(fn))
		cl.compileExprList(args)
		cl.emit(ir.Call(len(args)))
	}
}

func (cl *Compiler) compileReturn(form *sexp.Return) {
	switch len(form.Results) {
	case 0:
		cl.emit(ir.Return)
	case 1:
		cl.compileExpr(form.Results[0])
		cl.emit(ir.Return)

	default:
		panic("unimplemented") // #REFS: 1.
	}
}

func (cl *Compiler) compileIf(form *sexp.If) {
	cl.compileExpr(form.Cond)
	label := cl.emitJmp(ir.OpJmpNil, "then")
	cl.compileStmtList(form.Then.Forms)
	label.bind("else")
	if form.Else != nil {
		cl.compileStmt(form.Else)
	}
}

func (cl *Compiler) compileBlock(form *sexp.Block) {
	cl.compileStmtList(form.Forms)
	cl.symPool.Drop(form.Scope.Len())
	cl.emit(ir.Instr{Op: ir.OpScopeExit, Data: uint16(form.Scope.Len())})
}

func (cl *Compiler) compileBind(form *sexp.Bind) {
	cl.compileExpr(form.Init)
	id := cl.symPool.Insert(form.Name)
	cl.emit(ir.LocalBind(id))
}

func (cl *Compiler) compileRebind(form *sexp.Rebind) {
	cl.compileExpr(form.Expr)
	id := cl.symPool.Find(form.Name)
	cl.emit(ir.LocalSet(id))
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
	cl.emit(ir.Drop(1)) // Discard expression result.
}

func (cl *Compiler) compilePanic(errorData sexp.Form) {
	cl.emitConst(cl.constPool.InsertSym("Go--panic"))
	cl.compileExpr(errorData)
	cl.emit(ir.Panic)
	cl.code.pushBlock("panic")
}

func (cl *Compiler) compileVar(form sexp.Var) {
	// #FIXME: it could be a global var.
	id := cl.symPool.Find(form.Name)
	cl.code.pushInstr(ir.LocalRef(id))
}

func (cl *Compiler) compileTypeAssert(form *sexp.TypeAssert) {
	panic("unimplemented")
}

func (cl *Compiler) compileLispTypeAssert(form *sexp.LispTypeAssert) {
	// Bool type needs no assertion at all.
	if types.Identical(lisp.Types.Bool, form.Type) {
		return
	}

	var blamer lisp.Symbol // Panic trigger

	cl.compileExpr(form.Expr) // Arg to assert.

	if types.Identical(lisp.Types.Float, form.Type) {
		// For floats we do not have floatp opcode.
		cl.emitConst(cl.constPool.InsertSym("floatp"))
		cl.emit(ir.StackRef(1)) // Preserve arg (dup).
		cl.emit(ir.Call(1))
		blamer = "Go--!object-float"
	} else {
		var checker ir.Instr
		if types.Identical(lisp.Types.Int, form.Type) {
			checker = ir.IsInt
			blamer = "Go--!object-int"
		} else if types.Identical(lisp.Types.String, form.Type) {
			checker = ir.IsString
			blamer = "Go--!object-string"
		} else if types.Identical(lisp.Types.Symbol, form.Type) {
			checker = ir.IsSymbol
			blamer = "Go--!object-symbol"
		} else {
			panic("unimplemented")
		}

		cl.emit(ir.StackRef(0)) // Preserve arg (dup).
		cl.emit(checker)        // Type check.
	}

	label := cl.emitJmp(ir.OpJmpNotNil, "lisp-type-assert-fail")
	{
		cl.emitConst(cl.constPool.InsertSym(blamer))
		cl.emit(ir.StackRef(1)) // Value that failed assertion.
		cl.emit(ir.NoreturnCall(1))
	}
	label.bind("lisp-type-assert-pass")
}

func (cl *Compiler) compileConcat(form *sexp.Concat) {
	cl.compileExprList(form.Args)
	cl.emit(ir.Concat(len(form.Args)))
}

func (cl *Compiler) compileAddSub(op ir.Opcode, args []sexp.Form) {
	if !optAddSub(cl, op, args) {
		cl.compileOp(op, args)
	}
}

func (cl *Compiler) compileBool(form sexp.Bool) {
	if form.Val {
		cl.emitConst(cl.constPool.InsertSym("t"))
	} else {
		cl.emitConst(cl.constPool.InsertSym("nil"))
	}
}

func (cl *Compiler) compileWhile(form *sexp.While) {
	entryLabel := cl.emitJmp(ir.OpJmp, "loop-body")
	cl.compileStmtList(form.Body)

	entryLabel.bind("loop-test")
	cl.compileExpr(form.Cond)

	// #FIXME: nasty hack with +1. Should refactor.
	cl.emit(ir.JmpNotNil(entryLabel.blockIndex + 1))
}

func (cl *Compiler) compileExprStmt(form sexp.Form) {
	cl.compileExpr(form)
	cl.emit(ir.Drop(1)) // Discard expression result.
}

func (cl *Compiler) compileInstr(instr ir.Instr, argc int, args []sexp.Form) {
	if len(args) != argc {
		// #FIXME: need better error handling here.
		panic(fmt.Sprintf("%s expected %d args, got %d",
			instr.Op, argc, len(args)))
	}
	cl.compileExprList(args)
	cl.emit(instr)
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
	cl.code.pushInstr(instr)
}

func (cl *Compiler) emitConst(cpIndex int) {
	cl.emit(ir.ConstRef(cpIndex))
}

func (cl *Compiler) emitJmp(op ir.Opcode, branchName string) jmpLabel {
	label := cl.code.pushJmp(op)
	cl.code.pushBlock(branchName)
	return label
}

func (cl *Compiler) pushBlock(name string) {
	cl.code.pushBlock(name)
}

func (cl *Compiler) createObject() bytecode.Object {
	return bytecode.Object{
		Blocks:    cl.code.blocks,
		ConstPool: cl.constPool,
		Locals:    cl.symPool.Symbols(),
	}
}
