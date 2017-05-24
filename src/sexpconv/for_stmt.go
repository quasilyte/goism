package sexpconv

import (
	"go/ast"
	"go/token"
	"go/types"
	"sexp"
)

func (conv *Converter) RangeStmt(node *ast.RangeStmt) sexp.Form {
	switch typ := conv.typeOf(node.X).(type) {
	case *types.Array:
		return conv.foreachArray(node, typ)

	default:
		panic("unimplemented")
	}
}

func (conv *Converter) foreachArray(node *ast.RangeStmt, typ *types.Array) sexp.Form {
	if node.Tok == token.ASSIGN {
		// Only handling ":=" (DEFINE) for now.
		panic("unimplemented")
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
	panic("unimplemented")
}

func (conv *Converter) ForStmt(node *ast.ForStmt) sexp.Form {
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
			Scope: conv.info.Scopes[node],
		}
	}

	return loop
}
