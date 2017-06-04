package sexpconv

import (
	"exn"
	"go/ast"
	"sexp"
	"sys_info/function"
)

func (conv *Converter) callExprList(fn *sexp.Func, args []ast.Expr) *sexp.Call {
	return &sexp.Call{
		Fn:   fn,
		Args: conv.valueCopyList(conv.exprList(args)),
	}
}

func (conv *Converter) uniArgList(args []interface{}) []sexp.Form {
	forms := make([]sexp.Form, len(args))
	for i, arg := range args {
		if node, ok := arg.(ast.Expr); ok {
			forms[i] = conv.valueCopy(conv.Expr(node))
		} else {
			forms[i] = conv.valueCopy(arg.(sexp.Form))
		}
	}
	return forms
}

// Convenient function to generate function call node.
// Recognizes ast.Expr and sexp.Form as arguments.
func (conv *Converter) call(fn *sexp.Func, args ...interface{}) *sexp.Call {
	return &sexp.Call{Fn: fn, Args: conv.uniArgList(args)}
}

func (conv *Converter) lispCall(fn *function.LispFn, args ...interface{}) *sexp.LispCall {
	return &sexp.LispCall{Fn: fn, Args: conv.uniArgList(args)}
}

func (conv *Converter) CallExpr(node *ast.CallExpr) sexp.Form {
	// #REFS: 2.
	switch args := node.Args; fn := node.Fun.(type) {
	case *ast.SelectorExpr: // x.sel()
		sel := conv.info.Selections[fn]
		if sel != nil {
			panic(exn.NoImpl("method calls"))
		}

		pkg := fn.X.(*ast.Ident)
		if pkg.Name == "lisp" {
			return conv.intrinFuncCall(fn.Sel.Name, args)
		}

		// return conv.callExprList(conv.makeFunction(fn.Sel, pkg.Name), args)
		panic("!!!")

	case *ast.Ident: // f()
		switch fn.Name {
		// Unsigned types are not handled at this level.
		case "uint", "uint8", "byte", "uint16", "uint32", "uint64":
			return conv.Expr(args[0])
		// All signed integer types are treated as aliases.
		case "int", "int8", "int16", "int32", "rune", "int64":
			return conv.Expr(args[0])
		// All float types are considered float64
		case "float32", "float64":
			return conv.Expr(args[0])
		case "bool":
			return conv.Expr(args[0])
		case "string":
			return conv.lispCall(function.StrCast, args[0])
		case "make":
			return conv.makeBuiltin(args)
		case "len":
			return conv.lenBuiltin(args[0])
		case "cap":
			return conv.capBuiltin(args[0])
		case "append":
			return conv.appendBuiltin(args)
		case "copy":
			return conv.lispCall(function.SliceCopy, args[0], args[1])
		case "panic":
			return conv.lispCall(function.Panic, args[0])
		case "print":
			panic(errUnexpectedExpr(conv, node))
		case "println":
			panic(errUnexpectedExpr(conv, node))
		case "delete":
			return conv.lispCall(function.Remhash, args[1], args[0])
		default:
			return conv.callExprList(conv.env.Func(fn.Name), args)
		}

	default:
		panic(errUnexpectedExpr(conv, node))
	}
}
