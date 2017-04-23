package opt

import (
	"bytes"
	"emacs/sexp"
	"fmt"
	"go/types"
	"testing"
)

func inferType(x interface{}) types.Type {
	switch x := x.(type) {
	case int8, int16, int32, int64, int:
		return types.Typ[types.Int64]
	case string:
		return types.Typ[types.String]
	case sexp.Node:
		return x.Type()

	default:
		panic(fmt.Sprintf("can not infer type for %#v (%T)\n", x, x))
	}
}

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
	case float32:
		return sexp.Float{float64(x)}
	case float64:
		return sexp.Float{x}
	case string:
		if x[0] == 'f' {
			return sexp.Var{x, types.Typ[types.Float64]}
		}
		return sexp.Var{x, types.Typ[types.Int64]}
	case sexp.Node:
		return x
	default:
		panic("can not convert to sexp.Node")
	}
}

func variadicOp(typ sexp.OpKind, xs []interface{}) *sexp.Operation {
	args := make([]sexp.Node, len(xs))
	for i := range xs {
		args[i] = toNode(xs[i])
	}
	return &sexp.Operation{
		OpKind: typ,
		Args:   args,
		Typ:    inferType(args[0]).(*types.Basic),
	}
}

func add(xs ...interface{}) *sexp.Operation {
	return variadicOp(sexp.OpAdd, xs)
}

func sub(xs ...interface{}) *sexp.Operation {
	return variadicOp(sexp.OpSub, xs)
}

func mul(xs ...interface{}) *sexp.Operation {
	return variadicOp(sexp.OpMul, xs)
}

func div(xs ...interface{}) *sexp.Operation {
	return variadicOp(sexp.OpDiv, xs)
}

func rem(xs ...interface{}) *sexp.Operation {
	return variadicOp(sexp.OpRem, xs)
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
