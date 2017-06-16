package sexpconv

import (
	"exn"
	"go/ast"
	"go/types"
	"ir/instr"
	"magic_pkg/emacs/lisp"
	"magic_pkg/emacs/rt"
	"sexp"
	"sys_info/function"
)

func (conv *converter) callExprList(fn *sexp.Func, args []ast.Expr) *sexp.Call {
	return &sexp.Call{
		Fn:   fn,
		Args: conv.copyValuesList(conv.exprList(args)),
	}
}

func (conv *converter) uniArgList(args []interface{}) []sexp.Form {
	forms := make([]sexp.Form, len(args))
	for i, arg := range args {
		if node, ok := arg.(ast.Expr); ok {
			forms[i] = conv.copyValue(conv.Expr(node))
		} else {
			forms[i] = conv.copyValue(arg.(sexp.Form))
		}
	}
	return forms
}

// Convenient function to generate function call node.
// Recognizes ast.Expr and sexp.Form as arguments.
func (conv *converter) call(fn *sexp.Func, args ...interface{}) *sexp.Call {
	return &sexp.Call{Fn: fn, Args: conv.uniArgList(args)}
}

func (conv *converter) lispCall(fn *function.LispFn, args ...interface{}) *sexp.LispCall {
	return &sexp.LispCall{Fn: fn, Args: conv.uniArgList(args)}
}

func (conv *converter) CallExpr(node *ast.CallExpr) sexp.Form {
	// #REFS: 2.
	switch args := node.Args; fn := node.Fun.(type) {
	case *ast.SelectorExpr: // x.sel()
		sel := conv.info.Selections[fn]
		if sel != nil {
			recv := sel.Recv().(*types.Named)
			if recv == lisp.TypObject {
				return conv.lispObjectMethod(fn.Sel.Name, fn.X, args)
			}
			return conv.callExprList(
				conv.ftab.LookupMethod(recv.Obj(), fn.Sel.Name),
				append([]ast.Expr{fn.X}, args...),
			)
		}

		pkg := fn.X.(*ast.Ident)
		if pkg.Name == "lisp" {
			return conv.intrinFuncCall(fn.Sel.Name, args)
		}

		return conv.callOrCoerce(conv.info.ObjectOf(fn.Sel).Pkg(), fn.Sel, args)

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
			return conv.call(rt.FnBytesToStr, args[0])
		case "make":
			return conv.makeBuiltin(args)
		case "len":
			return conv.lenBuiltin(args[0])
		case "cap":
			return conv.capBuiltin(args[0])
		case "append":
			return conv.appendBuiltin(args)
		case "copy":
			dst, src := args[0], args[1]
			return conv.call(rt.FnSliceCopy, dst, src)
		case "panic":
			return conv.call(rt.FnPanic, args[0])
		case "print", "println":
			// #REFS: 35.
			argList := &sexp.InstrCall{
				Instr: instr.List(len(args)),
				Args:  conv.exprList(args),
			}
			if fn.Name == "print" {
				return conv.call(rt.FnPrint, argList)
			}
			return conv.call(rt.FnPrintln, argList)
		case "delete":
			key, m := args[0], args[1]
			return conv.lispCall(function.Remhash, m, key)

		default:
			return conv.callOrCoerce(conv.ftab.MasterPkg(), fn, args)
		}

	default:
		panic(errUnexpectedExpr(conv, node))
	}
}

func (conv *converter) callOrCoerce(p *types.Package, id *ast.Ident, args []ast.Expr) sexp.Form {
	fn := conv.ftab.LookupFunc(p, id.Name)
	if fn != nil {
		// Call.
		return conv.callExprList(fn, args)
	}
	// Coerce.
	arg := conv.Expr(args[0])
	dstTyp := arg.Type()
	typ := conv.typeOf(id)
	if _, ok := typ.Underlying().(*types.Basic); ok {
		return arg // #REFS: 25
	}
	if types.Identical(typ.Underlying(), dstTyp) {
		return arg // #REFS: 25
	}
	// #REFS: 44.
	panic(exn.NoImpl("struct conversions"))
}
