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

func TestFlatten(t *testing.T) {
	items := []testEntry{
		{
			add(1, 2, 3),
			"(+ 1 2 3)",
		},
		{
			add(1, add("x", 3)),
			"(+ 1 x 3)",
		},
		{
			add(1, 2, add(3, "x", 5), 6),
			"(+ 1 2 3 x 5 6)",
		},
		{
			add(add(1, 2), sub(3, 4), add(5, "x")),
			"(+ 1 2 (- 3 4) 5 x)",
		},
		{
			add("x", add("y", "z")),
			"(+ x y z)",
		},
	}

	for _, x := range items {
		given := nodeString(x.given)
		got := nodeString(opFlatten(x.given.(*sexp.VariadicOp)))
		if got != x.expected {
			blame(t, given, x.expected, got)
		}
	}
}

func TestConstFold(t *testing.T) {
	items := []testEntry{
		{add("x", "y"), "(+ x y)"},
		{add(1, 1), "2"},
		{add(1, 1, "x"), "(+ x 2)"},
		{add(2, sub("x", "y"), 3), "(+ (- x y) 5)"},
		{add(add(add(1, 1), 1), 1), "4"},

		{mul(2, 2, 1), "4"},
		{div(4, 2, 1), "2"},
		{mul(add(1, div(10, 2)), "x", "y"), "(* x y 6)"},

		{rem(4, 2), "0"},
		{rem(rem(5, add(1, 1, 1)), "x"), "(% 2 x)"},
	}

	for _, x := range items {
		given := nodeString(x.given)
		got := nodeString(ConstFold(x.given))
		if got != x.expected {
			blame(t, given, x.expected, got)
		}
	}
}
