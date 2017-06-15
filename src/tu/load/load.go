package load

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/build"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"magic_pkg/emacs/lisp"
	"magic_pkg/emacs/rt"
	"opt"
	"path/filepath"
	"reflect"
	"sexp"
	"sexpconv"
	"strings"
	"tu"
	"tu/symbols"
	"xast"
)

type unit struct {
	pkgs  []*xast.Package
	env   *symbols.Env
	funcs []*sexp.Func
	ftab  *symbols.FuncTable
	decls map[*sexp.Func]funcDeclData
	conv  *sexpconv.Converter
}

type funcDeclData struct {
	decl *ast.FuncDecl
	pkg  *xast.Package
}

type initData struct {
	vars []string
	init *sexp.Func
}

func newUnit(masterPkg *types.Package, pkgPath string) *unit {
	env := symbols.NewEnv(pkgPath)
	ftab := symbols.NewFuncTable(masterPkg)
	return &unit{
		env:   env,
		ftab:  ftab,
		funcs: make([]*sexp.Func, 0, 32),
		decls: make(map[*sexp.Func]funcDeclData, 32),
		conv:  sexpconv.NewConverter(ftab, env),
	}
}

func Runtime() error {
	pkgPath := build.Default.GOPATH + "/src/emacs/rt"
	pkg, err := translatePkg(pkgPath)
	if err != nil {
		return err
	}
	u := newUnit(pkg.TypPkg, pkgPath)
	u.pkgs = []*xast.Package{pkg}
	collectFuncs(u)
	rt.InitPackage(pkg.TypPkg)
	rt.InitFuncs(u.ftab)
	convertFuncs(pkg, u)
	u.ftab.ForEachFunc(opt.OptimizeFunc)
	return nil
}

func Package(pkgPath string, optimize bool) (*tu.Package, error) {
	masterPkg, err := translatePkg(pkgPath)
	if err != nil {
		return nil, err
	}
	u := newUnit(masterPkg.TypPkg, pkgPath)
	err = collectImports(u, masterPkg)
	if err != nil {
		return nil, err
	}

	collectFuncs(u)
	convertFuncs(masterPkg, u)
	if optimize {
		u.ftab.ForEachFunc(opt.OptimizeFunc)
	}

	initializers := collectInitializers(masterPkg, u.conv)
	return &tu.Package{
		Name:    masterPkg.AstPkg.Name,
		Funcs:   u.funcs,
		Init:    initializers.init,
		Vars:    initializers.vars,
		Comment: pkgComment(masterPkg.AstPkg.Files),
	}, nil
}

func convertFuncs(masterPkg *xast.Package, u *unit) {
	u.ftab.ForEach(func(p *types.Package, fn *sexp.Func) {
		data := u.decls[fn]
		fn.Body = u.conv.FuncBody(&xast.Func{
			Pkg:  data.pkg,
			Ret:  fn.Results,
			Body: data.decl.Body,
		})
		if p == masterPkg.TypPkg {
			u.funcs = append(u.funcs, fn)
		}
	})
}

func collectFuncs(u *unit) {
	for _, p := range u.pkgs {
		for _, f := range p.AstPkg.Files {
			for _, decl := range f.Decls {
				if decl, ok := decl.(*ast.FuncDecl); ok {
					collectFunc(u, p, decl)
				}
			}
		}
	}
}

func collectFunc(u *unit, p *xast.Package, decl *ast.FuncDecl) {
	sig := declSignature(p.Info, decl)
	fn := &sexp.Func{
		Name:      symbols.Mangle(p.FullName, decl.Name.Name),
		Params:    declParamNames(decl),
		DocString: decl.Doc.Text(),
		Variadic:  sig.Variadic(),
		Results:   resultTuple(sig),
	}
	u.ftab.InsertFunc(p.TypPkg, decl.Name.Name, fn)
	u.decls[fn] = funcDeclData{
		decl: decl,
		pkg:  p,
	}
}

func collectInitializers(p *xast.Package, conv *sexpconv.Converter) initData {
	body := make([]sexp.Form, 0, 16)
	vars := make([]string, 0, 8)
	env := conv.Env()

	blankIdent := &ast.Ident{Name: "_"}
	for _, init := range p.InitOrder {
		idents := make([]*ast.Ident, len(init.Lhs))
		for i, v := range init.Lhs {
			if v.Name() == "_" {
				idents[i] = blankIdent
			} else {
				idents[i] = &ast.Ident{Name: v.Name()}
				p.Uses[idents[i]] = v
				vars = append(vars, env.InternVar(nil, v.Name()))
			}
		}

		body = append(body, conv.VarInit(&xast.Assign{
			Pkg: p,
			Lhs: idents,
			Rhs: init.Rhs,
		}))
		if len(body) != 0 {
			body = append(body, &sexp.Return{})
		}

		// Clear information that was added to type info above.
		for _, ident := range idents {
			delete(p.Uses, ident)
		}
	}

	// InitOrder misses entries for variables without explicit
	// initializers. They are collected here.
	topScope := p.TypPkg.Scope()
	for _, name := range topScope.Names() {
		if v, ok := topScope.Lookup(name).(*types.Var); ok {
			if env.ContainsVar(v.Name()) {
				continue
			}
			sym := env.InternVar(nil, v.Name())
			vars = append(vars, sym)
			body = append(body, conv.VarZeroInit(sym, v.Type()))
		}
	}

	return initData{
		init: &sexp.Func{
			Name: "init",
			Body: &sexp.Block{Forms: body},
		},
		vars: vars,
	}
}

func collectImportsIter(pkgs *[]*xast.Package, p *xast.Package) error {
	*pkgs = append(*pkgs, p)
	for _, imp := range p.TypPkg.Imports() {
		if imp == lisp.Package {
			continue // "emacs/lisp" is a special package (lisp "unsafe")
		}

		pkgPath := build.Default.GOPATH + "/src/" + imp.Path()
		pkg, err := translatePkg(pkgPath)
		if err != nil {
			return err
		}
		collectImportsIter(pkgs, pkg)
		// We have to equal *types.Package objects in
		// pkg.TypPkg and imp. Some objects already bound
		// to imp pointer, so we replace pkg.TypPkg with it.
		pkg.TypPkg = imp
	}
	return nil
}

func collectImports(u *unit, p *xast.Package) error {
	u.pkgs = make([]*xast.Package, 0, 5)
	err := collectImportsIter(&u.pkgs, p)
	return err
}

func translatePkg(pkgPath string) (*xast.Package, error) {
	fset := token.NewFileSet()
	astPkg, err := parseDir(fset, pkgPath, parser.ParseComments)
	if err != nil {
		return nil, err
	}
	ti := &types.Info{
		Types:      make(map[ast.Expr]types.TypeAndValue),
		Defs:       make(map[*ast.Ident]types.Object),
		Uses:       make(map[*ast.Ident]types.Object),
		Selections: make(map[*ast.SelectorExpr]*types.Selection),
	}
	typPkg, err := typecheckPkg(fset, astPkg, ti)
	if err != nil {
		return nil, err
	}
	return &xast.Package{
		AstPkg:   astPkg,
		TypPkg:   typPkg,
		Info:     ti,
		FileSet:  fset,
		FullName: pkgFullName(pkgPath),
	}, nil
}

func pkgFullName(pkgPath string) string {
	offset := strings.Index(pkgPath, "emacs/") + len("emacs/")
	return pkgPath[offset:]
}

func parseDir(fset *token.FileSet, dir string, flags parser.Mode) (*ast.Package, error) {
	pkgs, err := parser.ParseDir(fset, dir, nil, flags)
	if err != nil {
		return nil, err
	}
	if len(pkgs) > 1 {
		pkgNames := reflect.ValueOf(pkgs).MapKeys()
		return nil, fmt.Errorf("more than one package found: %v", pkgNames)
	}
	// Get the first package (there are 0 or 1 map entries).
	for _, pkg := range pkgs {
		return pkg, nil
	}
	return nil, fmt.Errorf("can not find Go package in `%s'", dir)
}

var typecheckCfg = types.Config{
	Importer: &emacsImporter{impl: importer.Default()},
}

func typecheckPkg(fset *token.FileSet, pkg *ast.Package, ti *types.Info) (*types.Package, error) {
	// Convert file map to slice.
	files := make([]*ast.File, 0, len(pkg.Files))
	for _, file := range pkg.Files {
		files = append(files, file)
	}
	return typecheckCfg.Check(pkg.Name, fset, files, ti)
}

func pkgComment(files map[string]*ast.File) string {
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
