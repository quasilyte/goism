package tu

import (
	"go/ast"
	"go/types"
)

type goPackage struct {
	info *types.Info
	pkg  *ast.Package
}

func translatePackage(goPkg *goPackage) *Package {
	files := goPkg.pkg.Files
	pkg := &Package{Name: goPkg.pkg.Name}

	for _, file := range files {
		for _, decl := range file.Decls {
			translateDecl(pkg, goPkg.info, decl)
		}
	}

	return pkg
}

func translateDecl(pkg *Package, info *types.Info, decl ast.Decl) {
	switch decl := decl.(type) {
	case *ast.FuncDecl:
		translateFunc(pkg, info, decl)
	}
}

func translateFunc(pkg *Package, info *types.Info, decl *ast.FuncDecl) {
	visitor := &visitor{info: info}
	forms := visitor.visitStmtList(decl.Body.List)

	// Collect flat list of param names.
	params := decl.Type.Params
	paramNames := make([]string, 0, params.NumFields())
	for _, param := range params.List {
		for _, paramIdent := range param.Names {
			paramNames = append(paramNames, paramIdent.Name)
		}
	}

	pkg.Funcs = append(pkg.Funcs, &Func{
		Name:   decl.Name.Name,
		Params: paramNames,
		Body:   forms,
	})
}
