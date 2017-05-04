package prin1

import (
	"io"
	"sexp"
	"strconv"
	"unicode/utf8"
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

func (sw *sexpWriter) writeCharAtom(x rune) {
	sw.writeByte('?')
	n := utf8.EncodeRune(sw.buf[:4], x)
	sw.write(sw.buf[:n])
}

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

func (sw *sexpWriter) writeCall(f string, args []sexp.Form) {
	sw.writeByte('(')
	sw.writeString(f)
	for _, arg := range args {
		sw.writeByte(' ')
		sw.writeSexp(arg)
	}
	sw.writeByte(')')
}

func (sw *sexpWriter) writeSexp(object sexp.Form) {
	switch object := object.(type) {
	case sexp.Bool:
		if object.Val {
			sw.writeString("t")
		} else {
			sw.writeString("nil")
		}
	case sexp.Char:
		sw.writeCharAtom(object.Val)
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

	case *sexp.Assign:
		sw.writeString("(assign " + object.Name + " ")
		sw.writeSexp(object.Expr)
		sw.writeByte(')')
	case *sexp.Bind:
		sw.writeString("(bind " + object.Name + " ")
		sw.writeSexp(object.Init)
		sw.writeByte(')')

	case *sexp.FormList:
		sw.writeCall("form-list", object.Forms)
	case *sexp.Block:
		sw.writeCall("block", object.Forms)

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
	case *sexp.IntLt:
		sw.writeCall("<", object.Args)
	case *sexp.IntLte:
		sw.writeCall("<=", object.Args)
	case *sexp.IntGt:
		sw.writeCall(">", object.Args)
	case *sexp.IntGte:
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
	case *sexp.FloatLt:
		sw.writeCall("f<", object.Args)
	case *sexp.FloatLte:
		sw.writeCall("f<=", object.Args)
	case *sexp.FloatGt:
		sw.writeCall("f>", object.Args)
	case *sexp.FloatGte:
		sw.writeCall("f>=", object.Args)
	case *sexp.Concat:
		sw.writeCall("concat", object.Args)
	case *sexp.StringEq:
		sw.writeCall("s=", object.Args)
	case *sexp.StringNotEq:
		sw.writeCall("s!=", object.Args)
	case *sexp.StringLt:
		sw.writeCall("s<", object.Args)
	case *sexp.StringLte:
		sw.writeCall("s<=", object.Args)
	case *sexp.StringGt:
		sw.writeCall("s>", object.Args)
	case *sexp.StringGte:
		sw.writeCall("s>=", object.Args)

	case *sexp.Call:
		sw.writeCall(object.Fn, object.Args)
	}
}
