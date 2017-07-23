package sexpconv

import (
	"assert"
	"exn"
	"go/ast"
	"go/types"
	"magic_pkg/emacs/lisp"
	"magic_pkg/emacs/rt"
	"sexp"
	"xtypes"
)

func (conv *converter) apply(fn *sexp.Func, args []sexp.Form) *sexp.Call {
	conv.copyArgList(args, fn.InterfaceInputs)
	return &sexp.Call{Fn: fn, Args: args}
}

func (conv *converter) lispApply(fn *lisp.Func, args []sexp.Form) *sexp.LispCall {
	conv.copyArgList(args, nil)
	return &sexp.LispCall{Fn: fn, Args: args}
}

func (conv *converter) uniList(xs []interface{}) []sexp.Form {
	res := make([]sexp.Form, len(xs))
	for i, x := range xs {
		if node, ok := x.(ast.Expr); ok {
			res[i] = conv.Expr(node)
		} else {
			res[i] = x.(sexp.Form)
		}
	}
	return res
}

func (conv *converter) copyArgList(args []sexp.Form, ifaceTypes map[int]types.Type) {
	for i, arg := range args {
		args[i] = conv.copyValue(arg, ifaceTypes[i])
	}
}

// Convenient function to generate function call node.
// Recognizes ast.Expr and sexp.Form as arguments.
func (conv *converter) call(fn *sexp.Func, args ...interface{}) *sexp.Call {
	return conv.apply(fn, conv.uniList(args))
}

func (conv *converter) lispCall(fn *lisp.Func, args ...interface{}) *sexp.LispCall {
	return conv.lispApply(fn, conv.uniList(args))
}

func (conv *converter) typeCast(node ast.Expr, typ types.Type) *sexp.TypeCast {
	return &sexp.TypeCast{Form: conv.Expr(node), Typ: typ}
}

func (conv *converter) CallExpr(node *ast.CallExpr) sexp.Form {

	// #REFS: 2.
	switch args := node.Args; fn := node.Fun.(type) {
	case *ast.SelectorExpr: // x.sel()
		sel := conv.info.Selections[fn]
		if sel != nil {
			recv := xtypes.AsNamedType(sel.Recv())
			if recv == lisp.TypObject {
				return conv.lispObjectMethod(fn.Sel.Name, fn.X, args)
			}
			if !types.IsInterface(recv) {
				// Direct method call.
				return conv.apply(
					conv.ftab.LookupMethod(recv.Obj(), fn.Sel.Name),
					conv.exprList(append([]ast.Expr{fn.X}, args...)),
				)
			}
			// Interface (polymorphic) method call.
			if len(args) >= len(rt.FnIfaceCall) {
				panic(exn.NoImpl("interface method call with more than %d arguments", len(rt.FnIfaceCall)-1))
			}
			iface := recv.Underlying().(*types.Interface)
			return conv.apply(
				rt.FnIfaceCall[len(args)],
				append([]sexp.Form{
					conv.Expr(fn.X),
					sexp.Int(xtypes.LookupIfaceMethod(fn.Sel.Name, iface)),
				}, conv.exprList(args)...),
			)
		}

		pkg := fn.X.(*ast.Ident)
		if pkg.Name == "lisp" {
			return conv.intrinFuncCall(fn.Sel.Name, args)
		}

		return conv.callOrCoerce(conv.info.ObjectOf(fn.Sel).Pkg(), fn.Sel, args)

	case *ast.Ident: // f()
		switch fn.Name {
		case "uint":
			return conv.typeCast(args[0], xtypes.TypUint)
		case "uint8", "byte":
			return conv.typeCast(args[0], xtypes.TypUint8)
		case "uint16":
			return conv.typeCast(args[0], xtypes.TypUint16)
		case "uint32":
			return conv.typeCast(args[0], xtypes.TypUint32)
		case "uint64":
			return conv.typeCast(args[0], xtypes.TypUint64)

		case "int":
			return conv.typeCast(args[0], xtypes.TypInt)
		case "int8":
			return conv.typeCast(args[0], xtypes.TypInt8)
		case "int16":
			return conv.typeCast(args[0], xtypes.TypInt16)
		case "int32", "rune":
			return conv.typeCast(args[0], xtypes.TypInt32)
		case "int64":
			return conv.typeCast(args[0], xtypes.TypInt64)

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
			argList := &sexp.LispCall{
				Fn:   lisp.FnList,
				Args: conv.exprList(args),
			}
			if fn.Name == "print" {
				return conv.call(rt.FnPrint, argList)
			}
			return conv.call(rt.FnPrintln, argList)
		case "delete":
			key, m := args[0], args[1]
			return conv.lispCall(lisp.FnRemhash, m, key)

		default:
			return conv.callOrCoerce(conv.ftab.MasterPkg(), fn, args)
		}

	case *ast.ArrayType:
		// Currently, support only "[]byte(s)" expressions.
		typ := conv.typeOf(fn).(*types.Slice)
		elemTyp := typ.Elem().(*types.Basic)
		assert.True(elemTyp.Kind() == types.Byte)
		return conv.apply(rt.FnStrToBytes, conv.exprList(args))

	default:
		panic(errUnexpectedExpr(conv, node))
	}
}

func (conv *converter) callOrCoerce(p *types.Package, id *ast.Ident, args []ast.Expr) sexp.Form {
	fn := conv.ftab.LookupFunc(p, id.Name)
	if fn != nil {
		// Call.
		return conv.apply(fn, conv.exprList(args))
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
