package tu

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"lisp"
	"path/filepath"
	"reflect"
	"sexp"
	"sexpconv"
)

func translatePackage(pkgPath string) (pkg *Package, err error) {
	ctx, err := typecheckPkg(parsePkg(pkgPath))
	if err != nil {
		return nil, err
	}

	// Handle errors that can be thrown by AST convertion procedure.
	defer func() {
		switch panicArg := recover().(type) {
		case nil:
			return // No panic happened
		case sexpconv.Error:
			err = panicArg
			return // Return convertion error
		default:
			panic(panicArg) // Unexpected panic
		}
	}()
	pkg = convertPkg(ctx)
	pkg.Comment = packageComment(ctx.pkg.Files)
	return pkg, nil
}

type parseRes struct {
	fSet *token.FileSet
	pkg  *ast.Package
}

type typecheckRes struct {
	*parseRes
	ti       *types.Info
	topScope *types.Scope
}

func parsePkg(pkgPath string) (*parseRes, error) {
	const parseFlags = parser.ParseComments

	fSet := token.NewFileSet()

	pkgs, err := parser.ParseDir(fSet, pkgPath, nil, parseFlags)
	if err != nil {
		return nil, err
	}

	if len(pkgs) > 1 {
		pkgNames := reflect.ValueOf(pkgs).MapKeys()
		return nil, fmt.Errorf("more than one package found: %v", pkgNames)
	}
	// Get the first package (there are 0 or 1 map entries).
	for _, pkg := range pkgs {
		return &parseRes{fSet: fSet, pkg: pkg}, nil
	}
	return nil, fmt.Errorf("can not find Go package in %s", pkgPath)
}

func typecheckPkg(ctx *parseRes, parseErr error) (*typecheckRes, error) {
	if parseErr != nil {
		return nil, parseErr
	}

	cfg := types.Config{
		Importer: &emacsImporter{impl: importer.Default()},
	}
	ti := &types.Info{
		Types:      make(map[ast.Expr]types.TypeAndValue),
		Defs:       make(map[*ast.Ident]types.Object),
		Uses:       make(map[*ast.Ident]types.Object),
		Selections: make(map[*ast.SelectorExpr]*types.Selection),
	}

	// Convert file map to slice.
	files := make([]*ast.File, 0, len(ctx.pkg.Files))
	for _, file := range ctx.pkg.Files {
		files = append(files, file)
	}

	pkg, err := cfg.Check(ctx.pkg.Name, ctx.fSet, files, ti)
	return &typecheckRes{
		parseRes: ctx,
		ti:       ti,
		topScope: pkg.Scope(),
	}, err
}

func convertPkg(ctx *typecheckRes) *Package {
	pkg := &Package{
		Name: ctx.pkg.Name,
		Init: &Func{
			Name: "init",
			Body: &sexp.Block{},
		},
	}

	varsWithInit := make(map[string]struct{})
	conv := sexpconv.NewConverter(ctx.pkg.Name, ctx.ti, ctx.fSet)
	initForms := &pkg.Init.Body.Forms

	for _, init := range ctx.ti.InitOrder {
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

	for _, objName := range ctx.topScope.Names() {
		obj := ctx.topScope.Lookup(objName)

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
	for _, file := range ctx.pkg.Files {
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
