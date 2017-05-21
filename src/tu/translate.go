package tu

import (
	"go/ast"
	"go/types"
	"lisp"
	"sexp"
	"sexpconv"
)

func translatePackage(goPkg *goPackage, pkgComment string) *Package {
	pkg := &Package{
		Name:    goPkg.Name,
		Comment: pkgComment,
		Init: &Func{
			Name: "init",
			Body: &sexp.Block{},
		},
	}
	varsWithInit := make(map[string]struct{})
	conv := sexpconv.NewConverter(goPkg.info, goPkg.Name)
	initForms := &pkg.Init.Body.Forms

	for _, init := range goPkg.info.InitOrder {
		if len(init.Lhs) != 1 {
			// #FIXME: handle "x, y := f()" assignments.
			panic("unimplemented")
		}

		name := init.Lhs[0].Name()
		varsWithInit[name] = struct{}{}
		lispName := lisp.VarName(pkg.Name, name)

		pkg.Vars = append(pkg.Vars, lispName)
		form := conv.VarInit(lispName, init.Rhs)
		*initForms = append(*initForms, form)
	}

	topLevel := goPkg.topLevel
	for _, objName := range topLevel.Names() {
		obj := topLevel.Lookup(objName)

		switch obj.(type) {
		case *types.Var:
			// info.InitOrder misses entries for variables
			// without initializers. We need to collect them here.
			if _, ok := varsWithInit[objName]; !ok {
				name := lisp.VarName(pkg.Name, objName)
				pkg.Vars = append(pkg.Vars, name)
				form := conv.VarZeroInit(name, obj.Type())
				*initForms = append(*initForms, form)
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
