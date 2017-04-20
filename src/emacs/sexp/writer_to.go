package sexp

import (
	"fmt"
	"io"
)

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

func fprintf(w io.Writer, format string, a ...interface{}) (int64, error) {
	n, err := fmt.Fprintf(w, format, a...)
	return int64(n), err
}

func writeSexp(w io.Writer, head string, xs ...interface{}) (int64, error) {
	sw := sexpWriter{dst: w}

	sw.writeByte('(')
	sw.writeString(head)

	for _, x := range xs {
		sw.writeByte(' ')
		switch x := x.(type) {
		case []Node:
			for i := 0; i < len(x)-1; i++ {
				sw.writeWith(x[i])
				sw.writeByte(' ')
			}
			sw.writeWith(x[len(x)-1])
		case io.WriterTo:
			sw.writeWith(x)
		case string:
			sw.writeString(x)
		default:
			panic("Invalid value passed")
		}
	}

	sw.writeByte(')')

	return sw.written, sw.err
}

func (atom *Int) WriteTo(w io.Writer) (int64, error) {
	return fprintf(w, "%d", atom.Val)
}

func (atom *Float) WriteTo(w io.Writer) (int64, error) {
	return fprintf(w, "%f", atom.Val)
}

func (atom *String) WriteTo(w io.Writer) (int64, error) {
	return fprintf(w, "%s", atom.Val)
}

func (atom *Var) WriteTo(w io.Writer) (int64, error) {
	return fprintf(w, "%s", atom.Name)
}

func (block *Block) WriteTo(w io.Writer) (int64, error) {
	return writeSexp(w, "block", block.Nodes)
}

func (node *If) WriteTo(w io.Writer) (int64, error) {
	if node.Else == nil {
		return writeSexp(w, "if", node.Test, node.Then, "nil")
	}
	return writeSexp(w, "if", node.Test, node.Then, node.Else)
}

func (node *Bind) WriteTo(w io.Writer) (int64, error) {
	return writeSexp(w, "bind", node.Sym, " ", node.Val)
}

func (node *Return) WriteTo(w io.Writer) (int64, error) {
	return writeSexp(w, "return", node.Result)
}

var opTypeStrings = []string{
	OpAdd: "+",
	OpSub: "-",
	OpMul: "*",
	OpDiv: "/",
	OpEq:  "=",
	OpGt:  ">",
	OpGte: ">=",
	OpLt:  "<",
	OpLte: "<=",
}

func (op *VariadicOp) WriteTo(w io.Writer) (int64, error) {
	return writeSexp(w, opTypeStrings[op.Type], op.Args)
}

func (call *Call) WriteTo(w io.Writer) (int64, error) {
	return writeSexp(w, call.Fn, call.Args)
}
