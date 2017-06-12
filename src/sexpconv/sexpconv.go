package sexpconv

import (
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"sexp"
	"tu/symbols"
	"xast"
)

type Converter struct {
	info    *types.Info
	fileSet *token.FileSet
	env     *symbols.Env

	// Context type is used to resolve "untyped" constants.
	ctxType types.Type
	// Type that should be used for ctxType inside "return" statements.
	retType *types.Tuple
}

func NewConverter(env *symbols.Env, info *types.Info, fSet *token.FileSet) *Converter {
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

func (conv *Converter) VarInit(lhs []*ast.Ident, rhs ast.Expr) sexp.Form {
	return conv.genAssign(xast.ExprSlice(lhs), []ast.Expr{rhs})
}

func (conv *Converter) VarZeroInit(sym string, typ types.Type) sexp.Form {
	return &sexp.VarUpdate{Name: sym, Expr: ZeroValue(typ)}
}

func (conv *Converter) valueOf(node ast.Expr) constant.Value {
	return conv.info.Types[node].Value
}

func (conv *Converter) typeOf(node ast.Expr) types.Type {
	return conv.info.TypeOf(node)
}

func (conv *Converter) basicTypeOf(node ast.Expr) *types.Basic {
	return conv.info.TypeOf(node).Underlying().(*types.Basic)
}
