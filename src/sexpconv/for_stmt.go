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
			N:     sexp.Int(typ.Len()),
			Iter:  sexp.Var{Name: key.Name, Typ: keyTyp},
			Step:  sexp.Int(1),
			Body:  body,
			Scope: conv.info.Scopes[node],
		}
	}

	// For <KEY>, <VAL> := range <X>.
	panic(exn.NoImpl("for loop variant"))
}

func (conv *converter) ForStmt(node *ast.ForStmt) sexp.Form {
	body := conv.BlockStmt(node.Body)
	if node.Post != nil {
		body.Forms = append(body.Forms, conv.Stmt(node.Post))
	}

	var loop sexp.Form = &sexp.While{
		Cond: conv.Expr(node.Cond),
		Body: body,
	}

	if node.Init != nil {
		init := conv.Stmt(node.Init)
		loop = &sexp.Block{
			Forms: []sexp.Form{init, loop},
		}
	}

	return loop
}
