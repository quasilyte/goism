package load

import (
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"reflect"
	"sexp"
	"sexpconv"
	"tu"
)

func pkgToUnit(pkgPath string) (*unit, error) {
	parseRes, err := parsePkg(pkgPath)
	if err != nil {
		return nil, err
	}
	typecheckRes, err := typecheckPkg(parseRes)
	if err != nil {
		return nil, err
	}
	env := tu.NewEnv(pkgPath)
	return newUnit(parseRes, typecheckRes, env), nil
}

type parseRes struct {
	fSet   *token.FileSet
	astPkg *ast.Package
}

type typecheckRes struct {
	ti       *types.Info
	typesPkg *types.Package
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
		return &parseRes{fSet: fSet, astPkg: pkg}, nil
	}
	return nil, fmt.Errorf("can not find Go package in %s", pkgPath)
}

var typecheckCfg = types.Config{
	Importer: &emacsImporter{impl: importer.Default()},
}

func typecheckPkg(ctx *parseRes) (*typecheckRes, error) {

	ti := &types.Info{
		Types:      make(map[ast.Expr]types.TypeAndValue),
		Defs:       make(map[*ast.Ident]types.Object),
		Uses:       make(map[*ast.Ident]types.Object),
		Selections: make(map[*ast.SelectorExpr]*types.Selection),
	}

	// Convert file map to slice.
	files := make([]*ast.File, 0, len(ctx.astPkg.Files))
	for _, file := range ctx.astPkg.Files {
		files = append(files, file)
	}

	pkg, err := typecheckCfg.Check(ctx.astPkg.Name, ctx.fSet, files, ti)
	return &typecheckRes{
		ti:       ti,
		typesPkg: pkg,
		topScope: pkg.Scope(),
	}, err
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
