package tu

import (
	"go/ast"
	"go/importer"
	"go/token"
	"go/types"
)

type typecheckRes struct {
	info *types.Info
	pkg  *types.Package
}

var typecheckCfg = types.Config{
	Importer: &emacsImporter{
		impl: importer.Default(),
	},
}

func typecheck(fSet *token.FileSet, files []*ast.File) (typecheckRes, error) {
	info := &types.Info{
		Types:      make(map[ast.Expr]types.TypeAndValue),
		Defs:       make(map[*ast.Ident]types.Object),
		Uses:       make(map[*ast.Ident]types.Object),
		Selections: make(map[*ast.SelectorExpr]*types.Selection),
		Scopes:     make(map[ast.Node]*types.Scope),
	}

	pkg, err := typecheckCfg.Check("$PATH", fSet, files, info)
	return typecheckRes{info: info, pkg: pkg}, err
}
