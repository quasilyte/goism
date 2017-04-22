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
		{
			add(1.1, 1.1, 1.1),
			"(f+ 1.1 1.1 1.1)",
		},
		{
			add(1.1, add(1.1, 1.1)),
			"(f+ 1.1 1.1 1.1)",
		},
	}

	for _, x := range items {
		given := nodeString(x.given)
		got := nodeString(flattenIntVariadicOp(x.given.(*sexp.VariadicOp)))
		if got != x.expected {
			blame(t, given, x.expected, got)
		}
	}
}

func TestConstFoldInt(t *testing.T) {
	items := []testEntry{
		{add("x", "y"), "(+ x y)"},
		{add(1, 1), "2"},
		{add(1, 1, "x"), "(+ x 2)"},
		{add(2, sub("x", "y"), 3), "(+ (- x y) 5)"},
		{add(add(add(1, 1), 1), 1), "4"},
		{add(add(1, "x", "y", 1), "z", 1), "(+ x y z 3)"},

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

func TestConstFoldFloat(t *testing.T) {
	items := []testEntry{
		{add(1.5, 1.0), "2.5"},
		{sub(1.5, 1.0), "0.5"},
		{mul(2.1, 2.0), "4.2"},
		{div(4.2, 2.0), "2.1"},

		{add(1.1, add(1.1, 1.1, 1.1), 1.1), "5.5"},
		{mul(0.0, 2.0, 1.5), "0"},

		{add(1.1, 2.1, "fx"), "(f+ fx 3.2)"},
		{add(add(1.0, "fx", "fy", 1.0), "fz", 1.0), "(f+ fx fy fz 3)"},
	}

	for _, x := range items {
		given := nodeString(x.given)
		got := nodeString(ConstFold(x.given))
		if got != x.expected {
			blame(t, given, x.expected, got)
		}
	}
}
