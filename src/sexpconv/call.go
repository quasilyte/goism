package sexpconv

import (
	"fmt"
	"go/ast"
	"go/types"
	"lisp/function"
	"sexp"
)

func (conv *Converter) call(fn *function.Type, args ...ast.Expr) *sexp.Call {
	return &sexp.Call{
		Fn:   fn,
		Args: conv.valueCopyList(conv.exprList(args)),
	}
}

func (conv *Converter) CallExpr(node *ast.CallExpr) sexp.Form {
	// #REFS: 2.
	switch fn := node.Fun.(type) {
	case *ast.SelectorExpr: // x.sel()
		sel := conv.info.Selections[fn]
		if sel != nil {
			panic("method calls unimplemented")
		}

		pkg := fn.X.(*ast.Ident)
		if pkg.Name == "lisp" {
			return conv.intrinFuncCall(fn.Sel.Name, node.Args)
		}

		return conv.call(conv.makeFunction(fn.Sel, pkg.Name), node.Args...)

	case *ast.Ident: // f()
		switch fn.Name {
		// All signed integer types are treated as aliases.
		case "int", "int8", "int16", "int32", "rune", "int64":
			return conv.Expr(node.Args[0])
		// All float types are considered float64
		case "float32", "float64":
			return conv.Expr(node.Args[0])
		case "string":
			return conv.Expr(node.Args[0])
		case "make":
			return conv.makeBuiltin(node.Args)
		case "len":
			return conv.lenBuiltin(node.Args[0])
		case "cap":
			return conv.capBuiltin(node.Args[0])
		case "append":
			return conv.appendBuiltin(node.Args)
		case "copy":
			return conv.call(function.SliceCopy, node.Args...)
		case "panic":
			return &sexp.Panic{ErrorData: conv.Expr(node.Args[0])}
		case "print":
			return conv.call(function.Print, node.Args...)
		case "println":
			return conv.call(function.Println, node.Args...)
		case "delete":
			return conv.call(function.Remhash, node.Args[1], node.Args[0])
		default:
			return conv.call(conv.makeFunction(fn, ""), node.Args...)
		}

	default:
		panic(fmt.Sprintf("unexpected func: %#v", node.Fun))
	}
}

func (conv *Converter) makeFunction(fn *ast.Ident, pkgName string) *function.Type {
	// #FIXME: can also be *types.Names (type cast), etc.
	sig := conv.typeOf(fn).(*types.Signature)

	if pkgName == "" {
		qualName := conv.symPrefix + fn.Name
		return function.New(qualName, sig)
	}
	qualName := "Go-" + pkgName + "." + fn.Name
	return function.New(qualName, sig)
}
