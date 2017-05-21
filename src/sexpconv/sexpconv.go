package sexpconv

import (
	"go/ast"
	"go/constant"
	"go/types"
	"sexp"
)

type Converter struct {
	symPrefix string
	info      *types.Info

	// Context type is used to resolve "untyped" constants.
	ctxType types.Type
	// Type that should be used for ctxType inside "return" statements.
	retType *types.Tuple
}

func NewConverter(info *types.Info, pkgName string) *Converter {
	return &Converter{
		symPrefix: "Go-" + pkgName + ".",
		info:      info,
	}
}

func (conv *Converter) FuncBody(name *ast.Ident, block *ast.BlockStmt) *sexp.Block {
	conv.retType = conv.typeOf(name).(*types.Signature).Results()
	return conv.BlockStmt(block)
}

func (conv *Converter) VarInit(name string, node ast.Expr) sexp.Form {
	return &sexp.Rebind{Name: name, Expr: conv.Expr(node)}
}

func (conv *Converter) VarZeroInit(name string, typ types.Type) sexp.Form {
	return &sexp.Rebind{Name: name, Expr: ZeroValue(typ)}
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
