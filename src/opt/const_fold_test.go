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
		got := nodeString(flattenIntVariadicOp(x.given.(*sexp.Operation)))
		if got != x.expected {
			blame(t, given, x.expected, got)
		}
	}
}

func TestConstFoldInt(t *testing.T) {
	items := []testEntry{
		// Can not fold any constants:
		{add("x", "y"), "(+ x y)"},
		{add("x", 5, "y"), "(+ x y 5)"},
		{add("x", add("y", 1)), "(+ x y 1)"},

		// Commutative ops fold:
		{add(1, 2), "3"},
		{add(1, 2, "x"), "(+ x 3)"},
		{add(add(1, add(2, "x")), add(1, 2)), "(+ x 6)"},
		{mul("x", 2, 2, 2), "(* x 8)"},
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
		// Can not fold any constants:
		{add("fx", "fy"), "(f+ fx fy)"},
		{add("fx", 5.1, "fy"), "(f+ fx fy 5.1)"},
		{add("fx", add("fy", 1.1)), "(f+ fx fy 1.1)"},

		// Commutative ops fold:
		{add(1.1, 2.1), "3.2"},
		{add(1.1, 2.1, "fx"), "(f+ fx 3.2)"},
		{add(add(1.1, add(2.1, "fx")), add(1.1, 2.1)), "(f+ fx 6.4)"},
		{mul("fx", 2.0, 2.0, 2.0), "(f* fx 8)"},
	}

	for _, x := range items {
		given := nodeString(x.given)
		got := nodeString(ConstFold(x.given))
		if got != x.expected {
			blame(t, given, x.expected, got)
		}
	}
}
