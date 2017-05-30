package tu

import (
	"go/ast"
)

func convertFuncs(u *unit) {
	for _, file := range u.pkg.Files {
		for _, decl := range file.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				convertFunc(u, decl)
			}
		}
	}
}

func convertFunc(u *unit, decl *ast.FuncDecl) {
	// Collect flat list of param names.
	params := decl.Type.Params
	paramNames := make([]string, 0, params.NumFields())
	for _, param := range params.List {
		for _, paramIdent := range param.Names {
			paramNames = append(paramNames, paramIdent.Name)
		}
	}

	body := u.conv.FuncBody(decl.Name, decl.Body)

	u.funcs = append(u.funcs, &Func{
		Name:      u.env.Intern(decl.Name.Name),
		Params:    paramNames,
		Body:      body,
		DocString: decl.Doc.Text(),
	})
}
