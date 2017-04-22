package opt

import (
	"bytes"
	"emacs/sexp"
	"testing"
)

func nodeString(node sexp.Node) string {
	buf := bytes.Buffer{}
	_, err := node.WriteTo(&buf)
	if err != nil {
		panic(err)
	}
	return buf.String()
}

func toNode(x interface{}) sexp.Node {
	switch x := x.(type) {
	case bool:
		return sexp.Bool{x}
	case int:
		return sexp.Int{int64(x)}
	case int64:
		return sexp.Int{x}
	case float64:
		return sexp.Float{x}
	case string:
		return sexp.Var{x}
	case sexp.Node:
		return x
	default:
		panic("unexpected arg")
	}
}

func variadicOp(typ sexp.VariadicOpType, xs []interface{}) *sexp.VariadicOp {
	args := make([]sexp.Node, len(xs))
	for i := range xs {
		args[i] = toNode(xs[i])
	}
	return &sexp.VariadicOp{typ, args}
}

func binaryOp(typ sexp.BinaryOpType, arg1, arg2 interface{}) *sexp.BinaryOp {
	return &sexp.BinaryOp{typ, toNode(arg1), toNode(arg2)}
}

func add(xs ...interface{}) *sexp.VariadicOp {
	return variadicOp(sexp.OpAdd, xs)
}

func sub(xs ...interface{}) *sexp.VariadicOp {
	return variadicOp(sexp.OpSub, xs)
}

func mul(xs ...interface{}) *sexp.VariadicOp {
	return variadicOp(sexp.OpMul, xs)
}

func div(xs ...interface{}) *sexp.VariadicOp {
	return variadicOp(sexp.OpDiv, xs)
}

func rem(arg1, arg2 interface{}) *sexp.BinaryOp {
	return binaryOp(sexp.OpRem, arg1, arg2)
}

func blame(t *testing.T, given, expected, got string) {
	t.Errorf(
		"\n   given: %s\nexpected: %s\n     got: %s\n",
		given, expected, got,
	)
}

type testEntry struct {
	given    sexp.Node
	expected string
}
