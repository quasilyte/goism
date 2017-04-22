package opt

import (
	"emacs/sexp"
	"testing"
)

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
