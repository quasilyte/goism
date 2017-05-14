package compiler

import (
	"bcode"
	"bytes"
	"dt"
	"tu"
)

type Compiler struct {
	buf    bytes.Buffer
	cvec   *dt.ConstPool
	st     *dt.ExecutionStack
	labels []label
}

// New returns a reusable compiler that can translate
// parsed objects to Elisp bytecode objects.
func New() *Compiler {
	return &Compiler{cvec: &dt.ConstPool{}}
}

// Prepare compiler for re-use.
func (cl *Compiler) reset(bindings []string) {
	cl.buf.Truncate(0)
	cl.cvec.Clear()
	cl.st = dt.NewExecutionStack(bindings)
	cl.labels = cl.labels[:0]
}

// CompileFunc returns compiled function object.
func (cl *Compiler) CompileFunc(f *tu.Func) *bcode.Func {
	cl.reset(f.Params)

	compileStmtList(cl, f.Body)
	labelsResolve(cl)

	return &bcode.Func{
		ArgsDesc:   argsDescriptor(len(f.Params), f.Variadic),
		StackUsage: cl.st.MaxLen(),
		Bytecode:   cl.buf.Bytes(),
		DocString:  docString(f),
		ConstVec:   cl.cvec,
	}
}
