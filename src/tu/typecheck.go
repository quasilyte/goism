package tu

import (
	"go/ast"
	"go/importer"
	"go/token"
	"go/types"
)

type goPackage struct {
	*ast.Package
	info     *types.Info
	topLevel *types.Scope
	fileSet  *token.FileSet
}

var typeCheckCfg = types.Config{
	Importer: &emacsImporter{
		impl: importer.Default(),
	},
}

func typecheckPackage(fSet *token.FileSet, parsedPkg *ast.Package) (*goPackage, error) {
	info := &types.Info{
		Types:      make(map[ast.Expr]types.TypeAndValue),
		Defs:       make(map[*ast.Ident]types.Object),
		Uses:       make(map[*ast.Ident]types.Object),
		Selections: make(map[*ast.SelectorExpr]*types.Selection),
		Scopes:     make(map[ast.Node]*types.Scope),
	}

	files := make([]*ast.File, 0, len(parsedPkg.Files))
	for _, file := range parsedPkg.Files {
		files = append(files, file)
	}

	checkedPkg, err := typeCheckCfg.Check("$PATH", fSet, files, info)
	goPkg := &goPackage{
		Package:  parsedPkg,
		info:     info,
		topLevel: checkedPkg.Scope(),
		fileSet:  fSet,
	}
	return goPkg, err
}
