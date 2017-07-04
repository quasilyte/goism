package sexpconv

import (
	"go/ast"
	"go/token"
	"sexp"
	"xast"
)

func (conv *converter) Stmt(node ast.Stmt) sexp.Form {
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
	case *ast.ForStmt:
		return conv.ForStmt(node)
	case *ast.RangeStmt:
		return conv.RangeStmt(node)
	case *ast.SwitchStmt:
		return conv.SwitchStmt(node)
	case *ast.BranchStmt:
		return conv.BranchStmt(node)
	case *ast.LabeledStmt:
		return conv.LabeledStmt(node)
	case *ast.EmptyStmt:
		return sexp.EmptyStmt

	default:
		panic(errUnexpectedStmt(conv, node))
	}
}

func (conv *converter) IfStmt(node *ast.IfStmt) sexp.Form {
	test := conv.Expr(node.Cond)
	then := conv.BlockStmt(node.Body)
	form := &sexp.If{Cond: test, Then: then}
	if node.Else == nil {
		form.Else = sexp.EmptyStmt
	} else {
		form.Else = conv.Stmt(node.Else)
	}
	return conv.withInitStmt(node.Init, form)
}

// Used to complement statement with init SimpleStmt
// (switch and if statements).
func (conv *converter) withInitStmt(init ast.Stmt, form sexp.Form) sexp.Form {
	switch init := init.(type) {
	case nil:
		return form

	case *ast.AssignStmt:
		assigns := toFormList(conv.AssignStmt(init)).Forms
		return sexp.Block(append(assigns, form))

	default:
		return &sexp.FormList{
			Forms: []sexp.Form{conv.Stmt(init), form},
		}
	}
}

func (conv *converter) ReturnStmt(node *ast.ReturnStmt) *sexp.Return {
	// #FIXME: will not work for "naked" returns.
	results := make([]sexp.Form, len(node.Results))
	for i, node := range node.Results {
		conv.ctxType = conv.retType.At(i).Type()
		results[i] = conv.Expr(node)
	}

	return &sexp.Return{
		Results: conv.copyValuesList(results),
	}
}

func (conv *converter) BlockStmt(node *ast.BlockStmt) sexp.Block {
	return sexp.Block(conv.stmtList(node.List))
}

func (conv *converter) DeclStmt(node *ast.DeclStmt) sexp.Form {
	decl := node.Decl.(*ast.GenDecl)

	switch decl.Tok {
	case token.VAR:
		return conv.varDecl(decl)
	}

	panic(errUnexpectedStmt(conv, node))
}

func (conv *converter) varDecl(node *ast.GenDecl) *sexp.FormList {
	forms := make([]sexp.Form, 0, 1)

	for _, spec := range node.Specs {
		forms = conv.valueSpec(forms, spec.(*ast.ValueSpec))
	}

	return &sexp.FormList{Forms: forms}
}

func (conv *converter) valueSpec(forms []sexp.Form, spec *ast.ValueSpec) []sexp.Form {
	if len(spec.Values) == 0 {
		zv := ZeroValue(conv.typeOf(spec.Type))
		for _, ident := range spec.Names {
			if ident.Name != "_" {
				forms = append(forms, &sexp.Bind{Name: ident.Name, Init: zv})
			}
		}
	} else {
		lhs := xast.ExprSlice(spec.Names)
		forms = append(forms, conv.genAssign(lhs, spec.Values))
	}
	return forms
}

func (conv *converter) IncDecStmt(node *ast.IncDecStmt) sexp.Form {
	target := node.X.(*ast.Ident) // #FIXME: should be any "addressable".
	if node.Tok == token.INC {
		return &sexp.Rebind{
			Name: target.Name,
			Expr: sexp.NewAdd1(conv.Expr(target)),
		}
	}
	return &sexp.Rebind{
		Name: target.Name,
		Expr: sexp.NewSub1(conv.Expr(target)),
	}
}

func (conv *converter) ExprStmt(node *ast.ExprStmt) sexp.Form {
	return &sexp.ExprStmt{Expr: conv.Expr(node.X)}
}
