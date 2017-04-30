package prin1

import (
	"io"
	"sexp"
	"strconv"
)

type sexpWriter struct {
	written int64
	err     error
	dst     io.Writer

	// Scratch buffer.
	buf [32]byte
}

func (sw *sexpWriter) write(data []byte) {
	if sw.err == nil {
		n, err := sw.dst.Write(data)
		sw.err = err
		sw.written += int64(n)
	}
}

func (sw *sexpWriter) writeByte(x byte)     { sw.write([]byte{x}) }
func (sw *sexpWriter) writeString(x string) { sw.write([]byte(x)) }

func (sw *sexpWriter) writeIntAtom(x int64) {
	sw.writeString(strconv.FormatInt(x, 10))
}

func (sw *sexpWriter) writeFloatAtom(x float64) {
	sw.writeString(strconv.FormatFloat(x, 'f', -1, 64))
}

func (sw *sexpWriter) writeStringAtom(x string) {
	if len(x)+2 <= len(sw.buf) {
		// Fast path for short strings
		sw.buf[0] = '"'
		copy(sw.buf[1:], x)
		sw.buf[len(x)+1] = '"'
		sw.write(sw.buf[:len(x)+2])
	} else {
		sw.writeByte('(')
		sw.writeString(x)
		sw.writeByte(')')
	}
}

func (sw *sexpWriter) writeCall(f string, args []sexp.Node) {
	sw.writeByte('(')
	sw.writeString(f)
	for _, arg := range args {
		sw.writeByte(' ')
		sw.writeSexp(arg)
	}
	sw.writeByte(')')
}

func (sw *sexpWriter) writeSexp(object sexp.Node) {
	switch object := object.(type) {
	case sexp.Bool:
		if object.Val {
			sw.writeString("t")
		} else {
			sw.writeString("nil")
		}
	case sexp.Int:
		sw.writeIntAtom(object.Val)
	case sexp.Float:
		sw.writeFloatAtom(object.Val)
	case sexp.String:
		sw.writeStringAtom(object.Val)
	case sexp.Var:
		sw.writeString(object.Name)

	case *sexp.ArrayLit:
		sw.writeCall("array", object.Vals)
	case *sexp.QuotedArray:
		sw.writeCall("quoted-array", object.Vals)

	case *sexp.Block:
		// #FIXME:
		// Special forms are hard to print,
		// but this is the only problematic place at the moment,
		// so no attempts to refactor it were made.
		if len(object.Locals) == 0 {
			sw.writeCall("block ()", object.Nodes)
		} else {
			sw.writeString("(block (")
			for _, binding := range object.Locals {
				sw.writeByte('(')
				sw.writeString(binding.Name)
				sw.writeString(" . ")
				sw.writeSexp(binding.Init)
				sw.writeByte(')')
			}
			for _, node := range object.Nodes {
				sw.writeByte(' ')
				sw.writeSexp(node)
			}
			sw.writeByte(')')
		}
	case *sexp.If:
		sw.writeString("(if ")
		sw.writeSexp(object.Test)
		sw.writeByte(' ')
		sw.writeSexp(object.Then)
		if object.Else != nil {
			sw.writeByte(' ')
			sw.writeSexp(object.Else)
		}
		sw.writeByte(')')
	case *sexp.Return:
		sw.writeCall("return", object.Results)

	case *sexp.IntAdd:
		sw.writeCall("+", object.Args)
	case *sexp.IntSub:
		sw.writeCall("-", object.Args)
	case *sexp.IntMul:
		sw.writeCall("*", object.Args)
	case *sexp.IntDiv:
		sw.writeCall("/", object.Args)
	case *sexp.IntBitOr:
		sw.writeCall("bitor", object.Args)
	case *sexp.IntBitAnd:
		sw.writeCall("bitand", object.Args)
	case *sexp.IntBitXor:
		sw.writeCall("bitxor", object.Args)
	case *sexp.IntRem:
		sw.writeCall("%", object.Args)
	case *sexp.IntEq:
		sw.writeCall("=", object.Args)
	case *sexp.IntNotEq:
		sw.writeCall("!=", object.Args)
	case *sexp.IntLess:
		sw.writeCall("<", object.Args)
	case *sexp.IntLessEq:
		sw.writeCall("<=", object.Args)
	case *sexp.IntGreater:
		sw.writeCall(">", object.Args)
	case *sexp.IntGreaterEq:
		sw.writeCall(">=", object.Args)
	case *sexp.FloatAdd:
		sw.writeCall("f+", object.Args)
	case *sexp.FloatSub:
		sw.writeCall("f-", object.Args)
	case *sexp.FloatMul:
		sw.writeCall("f*", object.Args)
	case *sexp.FloatDiv:
		sw.writeCall("f/", object.Args)
	case *sexp.FloatEq:
		sw.writeCall("f=", object.Args)
	case *sexp.FloatNotEq:
		sw.writeCall("f!=", object.Args)
	case *sexp.FloatLess:
		sw.writeCall("f<", object.Args)
	case *sexp.FloatLessEq:
		sw.writeCall("f<=", object.Args)
	case *sexp.FloatGreater:
		sw.writeCall("f>", object.Args)
	case *sexp.FloatGreaterEq:
		sw.writeCall("f>=", object.Args)
	case *sexp.Concat:
		sw.writeCall("concat", object.Args)
	case *sexp.StringEq:
		sw.writeCall("s=", object.Args)
	case *sexp.StringNotEq:
		sw.writeCall("s!=", object.Args)
	case *sexp.StringLess:
		sw.writeCall("s<", object.Args)
	case *sexp.StringLessEq:
		sw.writeCall("s<=", object.Args)
	case *sexp.StringGreater:
		sw.writeCall("s>", object.Args)
	case *sexp.StringGreaterEq:
		sw.writeCall("s>=", object.Args)

	case *sexp.Call:
		sw.writeCall(object.Fn, object.Args)
	}
}
