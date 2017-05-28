package tu

import (
	"bytes"
	"go/ast"
	"go/token"
	"go/types"
	"lisp"
	"path/filepath"
	"sexp"
	"sexpconv"
)

func translatePackage(pkgPath string) (pkg *Package, err error) {
	fSet := token.NewFileSet()

	parsedPkg, err := parsePackage(fSet, pkgPath)
	if err != nil {
		return nil, err
	}

	checkedPkg, err := typecheckPackage(fSet, parsedPkg)
	if err != nil {
		return nil, err
	}

	defer func() {
		switch panicArg := recover().(type) {
		case nil:
			return
		case sexpconv.Error:
			pkg = nil
			err = panicArg
			return
		default:
			panic(panicArg)
		}
	}()
	pkgComment := packageComment(parsedPkg.Files)
	return convertPackage(checkedPkg, pkgComment), nil
}

func packageComment(files map[string]*ast.File) string {
	var buf bytes.Buffer
	buf.WriteString(";; ") // To avoid expensive prepend in the end.

	for name, file := range files {
		if file.Doc != nil {
			buf.WriteString("\t<")
			buf.WriteString(filepath.Base(name))
			buf.WriteString(">\n")
			buf.WriteString(file.Doc.Text())
		}
	}

	if buf.Len() == len(";; ") {
		return ""
	}

	// Remove trailing newline.
	buf.Truncate(buf.Len() - 1)

	// Properly format comment text.
	comment := bytes.Replace(buf.Bytes(), []byte(`"`), []byte(`\"`), -1)
	comment = bytes.Replace(comment, []byte("\n"), []byte("\n;; "), -1)

	return string(comment)
}

func convertPackage(goPkg *goPackage, pkgComment string) *Package {
	pkg := &Package{
		Name:    goPkg.Name,
		Comment: pkgComment,
		Init: &Func{
			Name: "init",
			Body: &sexp.Block{},
		},
	}
	varsWithInit := make(map[string]struct{})
	conv := sexpconv.NewConverter(goPkg.Name, goPkg.info, goPkg.fileSet)
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

	if len(*initForms) != 0 {
		*initForms = append(*initForms, &sexp.Return{})
	}

	// Collect functions.
	for _, file := range goPkg.Files {
		for _, decl := range file.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				convertFunc(pkg, conv, decl)
			}
		}
	}

	return pkg
}

func convertFunc(pkg *Package, conv *sexpconv.Converter, decl *ast.FuncDecl) {
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
