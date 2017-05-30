package xast

import (
	"go/ast"
	"reflect"
)

// ExprSlice converts any []T to []ast.Expr,
// where T is any type that implements ast.Expr.
func ExprSlice(nodes interface{}) []ast.Expr {
	s := reflect.ValueOf(nodes)
	res := make([]ast.Expr, s.Len())
	for i := range res {
		res[i] = s.Index(i).Interface().(ast.Expr)
	}
	return res
}
