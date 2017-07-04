package sexpconv

import (
	"exn"
	"go/ast"
	"go/token"
	"go/types"
	"sexp"
)

func (conv *converter) RangeStmt(node *ast.RangeStmt) sexp.Form {
	switch typ := conv.typeOf(node.X).(type) {
	case *types.Array:
		return conv.foreachArray(node, typ)

	default:
		panic(exn.NoImpl("for/range for %T", typ))
	}
}

func (conv *converter) foreachArray(node *ast.RangeStmt, typ *types.Array) sexp.Form {
	if node.Tok == token.ASSIGN {
		panic(exn.NoImpl("'=' assign in for initializer"))
	}

	body := conv.BlockStmt(node.Body)

	// for range <X>.
	if node.Value == nil && node.Key == nil {
		return &sexp.Repeat{N: typ.Len(), Body: body}
	}
	key := node.Key.(*ast.Ident)
	keyTyp := conv.basicTypeOf(key)

	// for <KEY> := range <X>.
	if node.Value == nil {
		return &sexp.DoTimes{
			N:    sexp.Int(typ.Len()),
			Iter: sexp.Local{Name: key.Name, Typ: keyTyp},
			Step: sexp.Int(1),
			Body: body,
		}
	}

	// For <KEY>, <VAL> := range <X>.
	panic(exn.NoImpl("for loop variant"))
}

func (conv *converter) ForStmt(node *ast.ForStmt) sexp.Form {
	var (
		post sexp.Form
		init sexp.Form
	)

	if node.Post == nil {
		post = sexp.EmptyForm
	} else {
		post = conv.Stmt(node.Post)
	}
	if node.Init == nil {
		init = sexp.EmptyForm
	} else {
		init = conv.Stmt(node.Init)
	}

	body := conv.BlockStmt(node.Body)

	if node.Cond == nil {
		return &sexp.Loop{
			Init: init,
			Post: post,
			Body: body,
		}
	}
	return &sexp.While{
		Init: init,
		Cond: conv.Expr(node.Cond),
		Post: post,
		Body: body,
	}
}
