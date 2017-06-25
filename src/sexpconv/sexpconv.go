package sexpconv

import (
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"sexp"
	"tu/symbols"
	"xast"
	"xtypes"
)

type Converter struct {
	ftab *symbols.FuncTable
	env  *symbols.Env
}

func (conv *Converter) FuncTable() *symbols.FuncTable {
	return conv.ftab
}

func (conv *Converter) Env() *symbols.Env {
	return conv.env
}

type converter struct {
	info    *types.Info
	fileSet *token.FileSet
	env     *symbols.Env
	ftab    *symbols.FuncTable

	// Context type is used to resolve "untyped" constants.
	ctxType types.Type
	// Type that should be used for ctxType inside "return" statements.
	retType *types.Tuple
}

func NewConverter(ftab *symbols.FuncTable, env *symbols.Env) *Converter {
	return &Converter{env: env, ftab: ftab}
}

func (conv *Converter) newConverter(p *xast.Package) converter {
	return converter{
		env:     conv.env,
		info:    p.Info,
		fileSet: p.FileSet,
		ftab:    conv.ftab,
	}
}

func (conv *Converter) VarInit(assign *xast.Assign) sexp.Form {
	c := conv.newConverter(assign.Pkg)
	return c.VarInit(assign.Lhs, assign.Rhs)
}

func (conv *Converter) FuncBody(fn *xast.Func) *sexp.Block {
	c := conv.newConverter(fn.Pkg)
	c.retType = fn.Ret
	body := c.BlockStmt(fn.Body)

	// Adding return statement.
	// It is needed in void functions without explicit "return".
	if fn.Ret == xtypes.EmptyTuple {
		body.Forms = append(body.Forms, &sexp.Return{})
	}

	return body
}

func (conv *Converter) VarZeroInit(sym string, typ types.Type) sexp.Form {
	return &sexp.VarUpdate{Name: sym, Expr: ZeroValue(typ)}
}

func (conv *converter) FuncBody(name *ast.Ident, block *ast.BlockStmt) *sexp.Block {
	conv.retType = conv.typeOf(name).(*types.Signature).Results()
	body := conv.BlockStmt(block)
	// Adding return statement.
	// It is needed in void functions without explicit "return".
	// In all other cases, optimizations will wipe it out.
	body.Forms = append(body.Forms, &sexp.Return{})
	return body
}

func (conv *converter) VarInit(lhs []*ast.Ident, rhs ast.Expr) sexp.Form {
	return conv.genAssign(xast.ExprSlice(lhs), []ast.Expr{rhs})
}

func (conv *converter) VarZeroInit(sym string, typ types.Type) sexp.Form {
	return &sexp.VarUpdate{Name: sym, Expr: ZeroValue(typ)}
}

func (conv *converter) valueOf(node ast.Expr) constant.Value {
	return conv.info.Types[node].Value
}

func (conv *converter) typeOf(node ast.Expr) types.Type {
	return conv.info.TypeOf(node)
}

func (conv *converter) basicTypeOf(node ast.Expr) *types.Basic {
	return conv.info.TypeOf(node).Underlying().(*types.Basic)
}
