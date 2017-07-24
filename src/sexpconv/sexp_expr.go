package sexpconv

import (
	"assert"
	"go/ast"
	"go/constant"
	"magic_pkg/emacs/lisp"
	"sexp"
)

func sexpConst(conv *converter, expr ast.Expr) sexp.Form {
	cv := conv.valueOf(expr)
	if cv == nil {
		return nil
	}
	if conv.typeOf(expr) == lisp.TypSymbol {
		return sexp.Symbol{Val: constant.StringVal(cv)}
	}

	switch cv.Kind() {
	case constant.Int:
		val, exact := constant.Int64Val(cv)
		assert.True(exact)
		return sexp.Int(val)

	case constant.Float:
		val, _ := constant.Float64Val(cv)
		return sexp.Float(val)

	case constant.String:
		return sexp.Str(constant.StringVal(cv))

	case constant.Bool:
		return sexp.Bool(constant.BoolVal(cv))

	case constant.Complex:
		panic(errComplexNumExpr(conv, expr))

	default:
		panic(errUnknownConstant(conv, expr))
	}
}
