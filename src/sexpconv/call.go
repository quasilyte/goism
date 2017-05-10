package sexpconv

import (
	"fmt"
	"go/ast"
	"go/types"
	"sexp"
)

func (conv *Converter) call(fn string, args ...ast.Expr) *sexp.Call {
	return &sexp.Call{Fn: fn, Args: conv.exprList(args)}
}

func (conv *Converter) methodCall(sel *types.Selection, recv ast.Expr, args []ast.Expr) sexp.Form {
	typ := sel.Recv().(*types.Named)
	tn := typ.Obj()
	pkg := tn.Pkg()

	qualName := "Go-" + pkg.Name() + "." + tn.Name() + "." + sel.Obj().Name()
	return conv.call(qualName, append(args, recv)...)
}

func (conv *Converter) CallExpr(node *ast.CallExpr) sexp.Form {
	// #REFS: 2.
	switch fn := node.Fun.(type) {
	case *ast.SelectorExpr: // x.sel()
		sel := conv.info.Selections[fn]
		if sel != nil {
			return conv.methodCall(sel, fn.X, node.Args)
		}

		obj := fn.X.(*ast.Ident)
		if obj.Name == "lisp" {
			return conv.intrinFuncCall(fn.Sel.Name, node.Args)
		}

		qualName := "Go-" + obj.Name + "." + fn.Sel.Name
		return conv.call(qualName, node.Args...)

	case *ast.Ident: // f()
		switch fn.Name {
		case "make":
			return conv.makeBuiltin(node.Args)
		case "panic":
			return &sexp.Panic{ErrorData: conv.Expr(node.Args[0])}
		case "int", "string", "float64":
			return conv.Expr(node.Args[0])
		case "print":
			return conv.call("Go--print", node.Args...)
		case "println":
			return conv.call("Go--println", node.Args...)
		default:
			return conv.call(conv.symPrefix+fn.Name, node.Args...)
		}
	default:
		panic(fmt.Sprintf("unexpected func: %#v", node.Fun))
	}
}
