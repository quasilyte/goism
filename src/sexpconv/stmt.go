package sexpconv

import (
	"fmt"
	"go/ast"
	"go/token"
	"sexp"
)

func (conv *Converter) Stmt(node ast.Stmt) sexp.Form {
	switch node := node.(type) {
	case *ast.IfStmt:
		return conv.IfStmt(node)
	case *ast.ReturnStmt:
		return conv.ReturnStmt(node)
	case *ast.BlockStmt:
		return conv.BlockStmt(node)
	case *ast.DeclStmt:
		return conv.DeclStmt(node)
	case *ast.AssignStmt:
		return conv.AssignStmt(node)
	case *ast.IncDecStmt:
		return conv.IncDecStmt(node)
	case *ast.ExprStmt:
		return conv.ExprStmt(node)

	default:
		panic(fmt.Sprintf("unexpected stmt: %#v\n", node))
	}
}

func (conv *Converter) IfStmt(node *ast.IfStmt) *sexp.If {
	if node.Init != nil {
		panic("unimplemented")
	}

	test := conv.Expr(node.Cond)
	then := conv.BlockStmt(node.Body)
	form := &sexp.If{Test: test, Then: then}
	if node.Else != nil {
		form.Else = conv.Stmt(node.Else)
	}

	return form
}

func (conv *Converter) ReturnStmt(node *ast.ReturnStmt) *sexp.Return {
	return &sexp.Return{Results: conv.exprList(node.Results)}
}

func (conv *Converter) BlockStmt(node *ast.BlockStmt) *sexp.Block {
	return &sexp.Block{
		Forms: conv.stmtList(node.List),
		Scope: conv.info.Scopes[node],
	}
}

func (conv *Converter) DeclStmt(node *ast.DeclStmt) sexp.Form {
	decl := node.Decl.(*ast.GenDecl)

	switch decl.Tok {
	case token.VAR:
		return conv.varDecl(decl)
	}

	panic("unimplemented")
}

func (conv *Converter) varDecl(node *ast.GenDecl) *sexp.FormList {
	forms := make([]sexp.Form, 0, 1)

	for _, spec := range node.Specs {
		forms = conv.valueSpec(forms, spec.(*ast.ValueSpec))
	}

	return &sexp.FormList{Forms: forms}
}

func (conv *Converter) valueSpec(forms []sexp.Form, spec *ast.ValueSpec) []sexp.Form {
	if len(spec.Values) == 0 {
		zv := zeroValue(conv.typeOf(spec.Type))
		for _, ident := range spec.Names {
			forms = append(forms, &sexp.Bind{
				Name: ident.Name,
				Init: zv,
			})
		}
	} else {
		for i, ident := range spec.Names {
			forms = append(forms, &sexp.Bind{
				Name: ident.Name,
				Init: conv.Expr(spec.Values[i]),
			})
		}
	}
	return forms
}

func (conv *Converter) IncDecStmt(node *ast.IncDecStmt) sexp.Form {
	// "x++" == "x = x + 1"
	// "x--" == "x = x - 1"

	target := node.X.(*ast.Ident) // #FIXME: should be any "addressable".
	var expr sexp.Form

	args := []sexp.Form{conv.Expr(target), sexp.Int{Val: 1}}
	if node.Tok == token.INC {
		expr = &sexp.NumAdd{Args: args}
	} else {
		expr = &sexp.NumSub{Args: args}
	}

	return &sexp.Rebind{
		Name: target.Name,
		Expr: expr,
	}
}

func (conv *Converter) ExprStmt(node *ast.ExprStmt) sexp.Form {
	form := conv.Expr(node.X)
	if form, ok := form.(*sexp.Panic); ok {
		return form
	}
	return sexp.ExprStmt{Form: form}
}
