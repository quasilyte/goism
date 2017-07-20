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

	"github.com/pkg/errors"
)

type unit struct {
	pkgs    []*xast.Package
	env     *symbols.Env
	ins     *symbols.FuncTableInserter
	itabEnv *symbols.ItabEnv
	decls   map[*sexp.Func]funcDeclData
	conv    *sexpconv.Converter
}

type funcDeclData struct {
	decl *ast.FuncDecl
	pkg  *xast.Package
}

type initData struct {
	vars []string
	init *sexp.Func
}

func newUnit(ftab *symbols.FuncTable, masterPkg *types.Package, pkgPath string) *unit {
	env := symbols.NewEnv(pkgPath)
	itabEnv := symbols.NewItabEnv(masterPkg)
	return &unit{
		env:     env,
		ins:     ftab.Inserter(),
		itabEnv: itabEnv,
		decls:   make(map[*sexp.Func]funcDeclData, 32),
		conv:    sexpconv.NewConverter(ftab, env, itabEnv),
	}
}

func Runtime() error {
	pkgPath := "emacs/rt"
	pkg, err := translatePkg(pkgPath)
	if err != nil {
		return err
	}
	ftab := symbols.NewFuncTable(pkg.TypPkg)
	u := newUnit(ftab, pkg.TypPkg, pkgPath)
	u.pkgs = []*xast.Package{pkg}
	collectFuncs(u)
	rt.InitPackage(pkg.TypPkg)
	rt.InitFuncs(ftab)
	funcs := u.ins.GetAllFuncs()
	convertFuncs(u, funcs, true)
	opt.OptimizeFuncs(funcs)
	return nil
}

func Package(pkgPath string, optimize bool) (*tu.Package, error) {
	if err := checkPkgPath(pkgPath); err != nil {
		return nil, errors.Wrapf(err, "translate `%s'", pkgPath)
	}
	masterPkg, err := translatePkg(pkgPath)
	if err != nil {
		return nil, err
	}
	ftab := symbols.NewFuncTable(masterPkg.TypPkg)
	u := newUnit(ftab, masterPkg.TypPkg, pkgPath)
	err = collectImports(u, masterPkg)
	if err != nil {
		return nil, err
	}

	collectFuncs(u)
	funcs := u.ins.GetAllFuncs()
	convertFuncs(u, funcs, optimize)
	if optimize {
		opt.OptimizeFuncs(funcs)
	}

	initializers := collectInitializers(u, masterPkg)

	return &tu.Package{
		Name:    masterPkg.AstPkg.Name,
		Funcs:   u.ins.GetMasterFuncs(),
		Init:    initializers.init,
		Vars:    initializers.vars,
		Comment: pkgComment(masterPkg.AstPkg.Files),
	}, nil
}

func convertFuncs(u *unit, funcs []*sexp.Func, optimize bool) {
	for _, fn := range funcs {
		data := u.decls[fn]
		fn.Body = u.conv.FuncBody(&xast.Func{
			Pkg:  data.pkg,
			Ret:  fn.Results,
			Body: data.decl.Body,
		})
		if optimize && !fn.IsNoinline() && isInlineable(fn) {
			fn.SetInlineable(true)
		}
	}
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

func parseFuncDocText(fn *sexp.Func, doc *ast.CommentGroup) string {
	if doc == nil {
		return ""
	}
	for _, line := range doc.List {
		if strings.HasPrefix(line.Text, "//goism:") {
			fn.LoadDirective(line.Text)
			line.Text = "//" // Clear comment line
		}
	}
	return doc.Text()
}

func getRecvType(recv *types.Var) *types.TypeName {
	typ := recv.Type()
	if ptr, ok := typ.(*types.Pointer); ok {
		return ptr.Elem().(*types.Named).Obj()
	}
	return typ.(*types.Named).Obj()
}

func collectFunc(u *unit, p *xast.Package, decl *ast.FuncDecl) {
	sig := declSignature(p.Info, decl)
	name := decl.Name.Name
	fn := &sexp.Func{
		Variadic: sig.Variadic(),
		Results:  resultTuple(sig),
	}
	fn.DocString = parseFuncDocText(fn, decl.Doc)
	if recv := sig.Recv(); recv == nil {
		// Function.
		fn.Params = make([]string, 0, decl.Type.Params.NumFields())
		fn.Name = symbols.Mangle(p.FullName, name)
		fillFuncParamsInfo(u, fn, sig)
		u.ins.Func(p.TypPkg, name, fn)
	} else {
		// Method.
		fn.Params = make([]string, 0, decl.Type.Params.NumFields()+1)
		fn.Params = append(fn.Params, recv.Name()) // "recv" param
		typ := getRecvType(recv)
		fn.Name = symbols.MangleMethod(p.FullName, typ.Name(), name)
		fillFuncParamsInfo(u, fn, sig)
		u.ins.Method(typ, name, fn)
	}
	u.decls[fn] = funcDeclData{
		decl: decl,
		pkg:  p,
	}
}

func fillFuncParamsInfo(u *unit, fn *sexp.Func, sig *types.Signature) {
	for i := 0; i < sig.Params().Len(); i++ {
		param := sig.Params().At(i)
		fn.Params = append(fn.Params, param.Name())

		if types.IsInterface(param.Type()) {
			if fn.InterfaceInputs == nil {
				fn.InterfaceInputs = make(map[int]types.Type, sig.Params().Len()-i)
			}
			fn.InterfaceInputs[i] = param.Type()
		}
	}
}

func collectInitializers(u *unit, p *xast.Package) initData {
	conv := u.conv
	body := make([]sexp.Form, 0, 16)
	vars := make([]string, 0, 8)
	env := conv.Env()

	// Initialize master package itabs.
	for _, itab := range u.itabEnv.GetMasterItabs() {
		vars = append(vars, itab.Name)
		iface := itab.Iface
		elems := make([]sexp.Form, iface.NumMethods()+1)
		elems[0] = sexp.Symbol{Val: p.FullName + "." + itab.ImplName}
		for i := 0; i < iface.NumMethods(); i++ {
			sym := symbols.MangleMethod(
				p.FullName,
				itab.ImplName,
				iface.Method(i).Name(),
			)
			elems[i+1] = sexp.Symbol{Val: sym}
		}
		body = append(body, &sexp.VarUpdate{
			Name: itab.Name,
			Expr: sexp.NewLispCall(
				lisp.FnVector,
				elems...,
			),
		})
	}

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

	if len(body) != 0 {
		body = append(body, &sexp.Return{})
	}

	return initData{
		init: &sexp.Func{
			Name: "init",
			Body: sexp.Block(body),
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

		pkg, err := translatePkg(imp.Path())
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
	pkgPath = build.Default.GOPATH + "/src/" + pkgPath
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

func isInlineable(fn *sexp.Func) bool {
	/*
		totalCost := 0

		for _, form := range fn.Body {
			cost := form.Cost()
			if cost == -1 {
				// Most probably has loops.
				return false
			}
			totalCost += cost
			if totalCost > cfg.CostInlineFilter {
				// Give it up due to complexity.
				return false
			}
		}

		return true
	*/
	return true // #REFS: 90
}
