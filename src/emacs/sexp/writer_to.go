package sexp

import (
	"fmt"
	"go/types"
	"io"
)

func (atom Bool) WriteTo(w io.Writer) (int64, error) {
	if atom.Val {
		return fprint(w, "t")
	}
	return fprint(w, "nil")
}

func (atom Int) WriteTo(w io.Writer) (int64, error) {
	return fprintf(w, "%d", atom.Val)
}

func (atom Float) WriteTo(w io.Writer) (int64, error) {
	return fprintf(w, "%#v", atom.Val)
}

func (atom String) WriteTo(w io.Writer) (int64, error) {
	return fprintf(w, "%s", atom.Val)
}

func (atom Var) WriteTo(w io.Writer) (int64, error) {
	return fprintf(w, "%s", atom.Name)
}

func (lit *ArrayLit) WriteTo(w io.Writer) (int64, error) {
	return writeList(w, "array", lit.Vals)
}

func (lit *QuotedArray) WriteTo(w io.Writer) (int64, error) {
	return writeList(w, "quoted-array", lit.Vals)
}

func (block *Block) WriteTo(w io.Writer) (int64, error) {
	return writeList(w, "block", block.Nodes)
}

func (node *If) WriteTo(w io.Writer) (int64, error) {
	if node.Else == nil {
		return writeList(w, "if", node.Test, node.Then, "nil")
	}
	return writeList(w, "if", node.Test, node.Then, node.Else)
}

func (node *Bind) WriteTo(w io.Writer) (int64, error) {
	return writeList(w, "bind", node.Sym, " ", node.Val)
}

func (node *Return) WriteTo(w io.Writer) (int64, error) {
	return writeList(w, "return", node.Result)
}

var intOpHead = [...]string{
	OpBitOr:  "bit-or",
	OpBitAnd: "bit-and",
	OpBitXor: "bit-xor",
	OpAdd:    "+",
	OpSub:    "-",
	OpMul:    "*",
	OpDiv:    "/",

	OpRem: "%",

	OpEq:        "=",
	OpNotEq:     "!=",
	OpLess:      "<",
	OpLessEq:    "<=",
	OpGreater:   ">",
	OpGreaterEq: ">=",
}

var floatOpHead = [...]string{
	OpAdd: "f+",
	OpSub: "f-",
	OpMul: "f*",
	OpDiv: "f/",

	OpEq:        "f=",
	OpNotEq:     "f!=",
	OpLess:      "f<",
	OpLessEq:    "f<=",
	OpGreater:   "f>",
	OpGreaterEq: "f>=",
}

var stringOpHead = [...]string{
	OpConcat: "s+",

	OpEq:        "s=",
	OpNotEq:     "s!=",
	OpLess:      "s<",
	OpLessEq:    "s<=",
	OpGreater:   "s>",
	OpGreaterEq: "s>=",
}

func (op *Operation) WriteTo(w io.Writer) (int64, error) {
	var head string
	switch op.Typ.Kind() {
	case types.Int64:
		head = intOpHead[op.OpKind]
	case types.Float64:
		head = floatOpHead[op.OpKind]
	case types.String:
		head = stringOpHead[op.OpKind]
	default:
		panic("unexpected type")
	}
	return writeList(w, head, op.Args)
}

func (call *Call) WriteTo(w io.Writer) (int64, error) {
	return writeList(w, call.Fn, call.Args)
}

type sexpWriter struct {
	written int64
	dst     io.Writer
	err     error
}

func (sw *sexpWriter) writeWith(w io.WriterTo) {
	if sw.err == nil {
		n, err := w.WriteTo(sw.dst)
		sw.written += n
		sw.err = err
	}
}

func (sw *sexpWriter) write(data []byte) {
	if sw.err == nil {
		n, err := sw.dst.Write(data)
		sw.written += int64(n)
		sw.err = err
	}
}

func (sw *sexpWriter) writeString(s string) {
	sw.write([]byte(s))
}

func (sw *sexpWriter) writeByte(b byte) {
	sw.write([]byte{b})
}

func (sw *sexpWriter) writeForm(form interface{}) {
	switch form := form.(type) {
	case []Node:
		if len(form) == 0 {
			return
		}
		for i := 0; i < len(form)-1; i++ {
			sw.writeWith(form[i])
			sw.writeByte(' ')
		}
		sw.writeWith(form[len(form)-1])
	case io.WriterTo:
		sw.writeWith(form)
	case string:
		sw.writeString(form)
	default:
		panic("Invalid value passed")
	}
}

func fprint(w io.Writer, data string) (int64, error) {
	n, err := w.Write([]byte(data))
	return int64(n), err
}

func fprintf(w io.Writer, format string, a ...interface{}) (int64, error) {
	n, err := fmt.Fprintf(w, format, a...)
	return int64(n), err
}

func writeList(w io.Writer, head string, xs ...interface{}) (int64, error) {
	sw := sexpWriter{dst: w}

	sw.writeByte('(')
	sw.writeString(head)
	for _, x := range xs {
		sw.writeByte(' ')
		sw.writeForm(x)
	}
	sw.writeByte(')')

	return sw.written, sw.err
}
