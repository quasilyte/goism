package tu

import (
	"go/ast"
	"go/types"
	"sexp"
	"sexpconv"
)

func translatePackage(goPkg *goPackage) *Package {
	pkg := &Package{Name: goPkg.Name}
	varsWithInit := make(map[string]struct{})
	conv := sexpconv.NewConverter(goPkg.info, goPkg.Name)

	for _, init := range goPkg.info.InitOrder {
		if len(init.Lhs) != 1 {
			// #FIXME: handle "x, y := f()" assignments.
			panic("unimplemented")
		}

		name := init.Lhs[0].Name()
		varsWithInit[name] = struct{}{}
		pkg.Vars = append(pkg.Vars, &Var{
			Name: name,
			Init: conv.Expr(init.Rhs),
		})
	}

	topLevel := goPkg.topLevel
	for _, objName := range topLevel.Names() {
		obj := topLevel.Lookup(objName)

		switch obj.(type) {
		case *types.Var:
			// info.InitOrder misses entries for variables
			// without initializers. We need to collect them here.
			if _, ok := varsWithInit[objName]; !ok {
				pkg.Vars = append(pkg.Vars, &Var{Name: objName, Init: nil})
			}

		case *types.TypeName:
			// #FIXME: collect types.
			panic("unimplemented")
		}
	}

	// Collect functions.
	for _, file := range goPkg.Files {
		for _, decl := range file.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				translateFunc(pkg, conv, decl)
			}
		}
	}

	return pkg
}

func translateFunc(pkg *Package, conv *sexpconv.Converter, decl *ast.FuncDecl) {
	// Collect flat list of param names.
	params := decl.Type.Params
	paramNames := make([]string, 0, params.NumFields())
	for _, param := range params.List {
		for _, paramIdent := range param.Names {
			paramNames = append(paramNames, paramIdent.Name)
		}
	}

	body := conv.FuncBody(decl.Name, decl.Body)
	// Adding return statement.
	// It is needed in void functions without explicit "return".
	// In all other cases, optimizations will wipe it out.
	body.Forms = append(body.Forms, &sexp.Return{})

	pkg.Funcs = append(pkg.Funcs, &Func{
		Name:      "Go-" + pkg.Name + "." + decl.Name.Name,
		Params:    paramNames,
		Body:      body,
		DocString: decl.Doc.Text(),
	})
}
