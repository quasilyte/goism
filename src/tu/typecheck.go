package tu

import (
	"go/ast"
	"go/importer"
	"go/token"
	"go/types"
)

var typeCheckCfg = types.Config{
	Importer: emacsImporter{
		impl: importer.Default(),
	},
}

func typecheckPackage(fSet *token.FileSet, parsedPkg *ast.Package) (*goPackage, error) {
	info := &types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
	}

	files := make([]*ast.File, 0, len(parsedPkg.Files))
	for _, file := range parsedPkg.Files {
		files = append(files, file)
	}

	checkedPkg, err := typeCheckCfg.Check("$PATH", fSet, files, info)
	goPkg := &goPackage{
		info:     info,
		pkg:      parsedPkg,
		topLevel: checkedPkg.Scope(),
	}
	return goPkg, err
}
