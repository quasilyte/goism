package load

import (
	"bytes"
	"exn"
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"path/filepath"
	"reflect"
	"sexp"
	"sexpconv"
	"tu"
)

func translatePackage(pkgPath string) (pkg *tu.Package, err error) {
	parseRes, err := parsePkg(pkgPath)
	if err != nil {
		return nil, err
	}
	typecheckRes, err := typecheckPkg(parseRes)
	if err != nil {
		return nil, err
	}
	env := tu.NewEnv(parseRes.pkg.Name)
	unit := newUnit(parseRes, typecheckRes, env)

	// Handle errors that can be thrown by AST convertion procedure.
	defer func() { err = exn.Catch(recover()) }()
	convertInitializers(unit)
	convertFuncs(unit)
	comment := packageComment(unit.pkg.Files)
	return newPackage(unit, comment), nil
}

type parseRes struct {
	fSet *token.FileSet
	pkg  *ast.Package
}

type typecheckRes struct {
	ti       *types.Info
	topScope *types.Scope
}

type unit struct {
	*parseRes
	*typecheckRes
	conv *sexpconv.Converter

	env   *tu.Env
	vars  []string
	init  *sexp.Func
	funcs []*sexp.Func
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

func typecheckPkg(ctx *parseRes) (*typecheckRes, error) {
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
	return &typecheckRes{ti: ti, topScope: pkg.Scope()}, err
}

func newUnit(parseRes *parseRes, typecheckRes *typecheckRes, env *tu.Env) *unit {
	fSet := parseRes.fSet
	ti := typecheckRes.ti
	return &unit{
		parseRes:     parseRes,
		typecheckRes: typecheckRes,
		conv:         sexpconv.NewConverter(env, ti, fSet),
		env:          env,
	}
}

func newPackage(u *unit, comment string) *tu.Package {
	return &tu.Package{
		Name:    u.pkg.Name,
		Vars:    u.vars,
		Funcs:   u.funcs,
		Init:    u.init,
		Comment: comment,
	}
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
