package sexpconv

import (
	"go/ast"
	"go/constant"
	"go/types"
)

type Converter struct {
	symPrefix string
	info      *types.Info
}

func NewConverter(info *types.Info, pkgName string) *Converter {
	return &Converter{
		symPrefix: "Go-" + pkgName + ".",
		info:      info,
	}
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
