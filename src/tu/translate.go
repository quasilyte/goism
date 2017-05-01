package tu

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/types"
	"sexp"
)

type goPackage struct {
	info     *types.Info
	pkg      *ast.Package
	topLevel *types.Scope
}

func translatePackage(goPkg *goPackage) *Package {
	pkg := &Package{
		Name:      goPkg.pkg.Name,
		Constants: make(map[string]sexp.Node),
	}

	// Collect global constants and types.
	topLevel := goPkg.topLevel
	for _, objName := range topLevel.Names() {
		obj := topLevel.Lookup(objName)

		switch obj := obj.(type) {
		case *types.Const:
			pkg.Constants[objName] = translateConstValue(obj.Val())

		case *types.TypeName:
			panic("unimplemented")
		}
	}

	// Collect global vars.
	// #TODO

	// Collect functions.
	for _, file := range goPkg.pkg.Files {
		for _, decl := range file.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				translateFunc(pkg, goPkg.info, decl)
			}
		}
	}

	return pkg
}

func translateFunc(pkg *Package, info *types.Info, decl *ast.FuncDecl) {
	visitor := &visitor{
		info:         info,
		globalValues: pkg.Constants,
	}
	forms := visitor.visitStmtList(decl.Body.List)

	// Collect flat list of param names.
	params := decl.Type.Params
	paramNames := make([]string, 0, params.NumFields())
	for _, param := range params.List {
		for _, paramIdent := range param.Names {
			paramNames = append(paramNames, paramIdent.Name)
		}
	}

	pkg.Funcs = append(pkg.Funcs, &Func{
		Name:   decl.Name.Name,
		Params: paramNames,
		Body:   forms,
	})
}

func translateConstValue(val constant.Value) sexp.Node {
	switch val.Kind() {
	case constant.Int:
		val, _ := constant.Int64Val(val)
		return sexp.Int{Val: val}

	case constant.Float:
		val, _ := constant.Float64Val(val)
		return sexp.Float{Val: val}

	case constant.String:
		return sexp.String{Val: constant.StringVal(val)}

	default:
		panic(fmt.Sprintf("unexpected constant: %#v", val))
	}
}
