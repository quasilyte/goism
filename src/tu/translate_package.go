package tu

import (
	"go/ast"
	"go/token"
	"xast"
)

func collectDecl(pkg *Package, decl ast.Decl) {
	if decl, ok := decl.(*ast.FuncDecl); ok {
		if decl.Type.Results == nil {
			// Adding a trailing return statement.
			// If there are also explicit return at the end,
			// optimizer will wipe dead code out.
			decl.Body.List = append(decl.Body.List, &ast.ReturnStmt{})
		}
		pkg.Funcs = append(pkg.Funcs, &Func{Decl: decl})
		return
	}

	switch genDecl := decl.(*ast.GenDecl); genDecl.Tok {
	case token.VAR:
		for _, spec := range genDecl.Specs {
			for _, ident := range spec.(*ast.ValueSpec).Names {
				pkg.Vars = append(pkg.Vars, ident.Name)
			}
		}
	}
}

func translatePackage(pkgPath string) (*Package, error) {
	fSet := token.NewFileSet()

	pkg, err := parse(fSet, pkgPath)
	if err != nil {
		return nil, err
	}

	res := &Package{
		Name:    pkg.Name,
		FileSet: fSet,
		Funcs:   make([]*Func, 0, 32),
		Vars:    make([]string, 0, 16),
	}

	files := make([]*ast.File, 0, len(pkg.Files))
	for _, file := range pkg.Files {
		files = append(files, file)

		for _, decl := range file.Decls {
			collectDecl(res, decl)
		}
	}

	typecheckRes, err := typecheck(fSet, files)
	if err != nil {
		return nil, err
	}
	res.TypeInfo = typecheckRes.info

	for _, fn := range res.Funcs {
		fn.Tree = xast.NewTree(res.TypeInfo, fn.Decl.Body)
	}

	return res, nil
}
