package compiler

import (
	"bytes"
	"dt"
	"go/types"
	"ir"
	"tu"
	"xast"
)

type Compiler struct {
	ast *xast.Tree

	*types.Info

	cvec *dt.ConstPool
	st   *dt.ExecutionStack

	buf bytes.Buffer // IR output is written here

	lastLabelID int // Unique jmp label ID counter

	pkg *tu.Package
}

func New(pkg *tu.Package) *Compiler {
	return &Compiler{
		Info: pkg.TypeInfo,

		cvec: &dt.ConstPool{},
		st:   &dt.ExecutionStack{},

		pkg: pkg,
	}
}

func (cl *Compiler) CompileFunc(fn *tu.Func) *ir.Object {
	cl.reset(fn)

	preprocessTree(fn.Tree)
	cl.compileStmtList(fn.Root.List)

	return &ir.Object{
		StackUsage: cl.st.MaxLen(),
		Code:       cl.buf.Bytes(),
		ConstVec:   cl.cvec,
	}
}

// Prepare compiler for re-use.
func (cl *Compiler) reset(fn *tu.Func) {
	cl.ast = fn.Tree
	cl.buf.Truncate(0)
	cl.cvec.Clear()
	cl.st.Clear()

	cl.initStack(fn)
}

func (cl *Compiler) initStack(fn *tu.Func) {
	for _, field := range fn.Decl.Type.Params.List {
		for _, ident := range field.Names {
			cl.st.PushVar(ident.Name)
		}
	}
}
