package load

import (
	"go/ast"
	"go/types"
	"lisp/function"
	"tu"
)

var emptyTuple = types.NewTuple()

func convertFuncs(u *unit) {
	decls := make([]*ast.FuncDecl, 0, 16)

	for _, file := range u.pkg.Files {
		for _, decl := range file.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				decls = append(decls, decl)

				sig := u.ti.Defs[decl.Name].Type().(*types.Signature)
				typ := function.New(decl.Name.Name, sig)
				u.funcs = append(u.funcs, &tu.Func{Typ: typ})
			}
		}
	}

	u.env.SetFuncs(u.funcs)

	for i, decl := range decls {
		// Collect flat list of param names.
		params := decl.Type.Params
		paramNames := make([]string, 0, params.NumFields())
		for _, param := range params.List {
			for _, paramIdent := range param.Names {
				paramNames = append(paramNames, paramIdent.Name)
			}
		}

		fn := u.funcs[i]
		fn.Name = u.env.InternVar(decl.Name.Name)
		fn.Params = paramNames
		fn.Body = u.conv.FuncBody(decl.Name, decl.Body)
		fn.DocString = decl.Doc.Text()
	}
}
