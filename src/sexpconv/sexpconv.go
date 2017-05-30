package sexpconv

import (
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"lisp"
	"sexp"
)

type Converter struct {
	info    *types.Info
	fileSet *token.FileSet
	env     *lisp.Env

	// Context type is used to resolve "untyped" constants.
	ctxType types.Type
	// Type that should be used for ctxType inside "return" statements.
	retType *types.Tuple
}

func NewConverter(env *lisp.Env, info *types.Info, fSet *token.FileSet) *Converter {
	return &Converter{
		env:     env,
		info:    info,
		fileSet: fSet,
	}
}

func (conv *Converter) FuncBody(name *ast.Ident, block *ast.BlockStmt) *sexp.Block {
	conv.retType = conv.typeOf(name).(*types.Signature).Results()
	body := conv.BlockStmt(block)
	// Adding return statement.
	// It is needed in void functions without explicit "return".
	// In all other cases, optimizations will wipe it out.
	body.Forms = append(body.Forms, &sexp.Return{})
	return body
}

func (conv *Converter) VarInit(lhs []string, rhs ast.Expr) sexp.Form {
	if len(lhs) == 1 {
		return &sexp.VarUpdate{Name: lhs[0], Expr: conv.Expr(rhs)}
	}
	list := &sexp.FormList{Forms: make([]sexp.Form, len(lhs))}
	for i, form := range conv.rhsMultiValues(rhs) {
		if lhs[i] == "_" {
			list.Forms[i] = conv.ignoredExpr(form)
		} else {
			list.Forms[i] = &sexp.VarUpdate{Name: lhs[i], Expr: form}
		}
	}
	return list
}

func (conv *Converter) VarZeroInit(name string, typ types.Type) sexp.Form {
	return &sexp.VarUpdate{Name: name, Expr: ZeroValue(typ)}
}

func (conv *Converter) valueOf(node ast.Expr) constant.Value {
	return conv.info.Types[node].Value
}

func (conv *Converter) typeOf(node ast.Expr) types.Type {
	return conv.info.TypeOf(node)
}

func (conv *Converter) basicTypeOf(node ast.Expr) *types.Basic {
	return conv.info.TypeOf(node).(*types.Basic)
}
