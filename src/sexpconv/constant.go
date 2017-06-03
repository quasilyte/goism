package sexpconv

import (
	"assert"
	"go/ast"
	"go/constant"
	"go/types"
	"lisp"
	"sexp"
)

func (conv *Converter) Constant(node ast.Expr) sexp.Form {
	if cv := conv.valueOf(node); cv != nil {
		typ := conv.typeOf(node)
		if types.Identical(typ, lisp.TypSymbol) {
			return sexp.Symbol{Val: constant.StringVal(cv)}
		}

		switch cv.Kind() {
		case constant.Int:
			return constantInt(cv)
		case constant.Float:
			return constantFloat(cv)
		case constant.String:
			return constantString(cv)
		case constant.Bool:
			return constantBool(cv)

		default:
			panic(errUnexpectedExpr(conv, node))
		}
	}

	return nil
}

func constantString(cv constant.Value) sexp.Str {
	return sexp.Str(constant.StringVal(cv))
}

func constantInt(cv constant.Value) sexp.Int {
	val, exact := constant.Int64Val(cv)
	assert.True(exact)
	return sexp.Int(val)
}

func constantFloat(cv constant.Value) sexp.Float {
	val, _ := constant.Float64Val(cv)
	return sexp.Float(val)
}

func constantBool(cv constant.Value) sexp.Bool {
	return sexp.Bool(constant.BoolVal(cv))
}
