package load

import (
	"go/ast"
	"go/types"
	"sexp"
)

var emptyTuple = types.NewTuple()

func pkgFuncs(pkg *ast.Package) []*ast.FuncDecl {
	decls := make([]*ast.FuncDecl, 0, 16)

	for _, file := range pkg.Files {
		for _, decl := range file.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				decls = append(decls, decl)
			}
		}
	}

	return decls
}

func collectFuncs(u *unit, decls []*ast.FuncDecl) {
	for _, decl := range decls {
		sig := declSignature(u.ti, decl)
		fn := &sexp.Func{Results: resultTuple(sig)}
		u.funcs = append(u.funcs, fn)
		u.env.AddFunc(decl.Name.Name, fn)
	}
}

func convertFuncs(u *unit, decls []*ast.FuncDecl) {
	for i, decl := range decls {
		fn := u.funcs[i]
		fn.Name = u.env.InternVar(nil, decl.Name.Name)
		fn.Params = declParamNames(decl)
		fn.Body = u.conv.FuncBody(decl.Name, decl.Body)
		fn.DocString = decl.Doc.Text()
	}
}
