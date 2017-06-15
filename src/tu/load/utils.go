package load

import (
	"go/ast"
	"go/types"
	"tu/symbols"
	"xast"
	"xtypes"
)

func declSignature(ti *types.Info, decl *ast.FuncDecl) *types.Signature {
	return ti.Defs[decl.Name].Type().(*types.Signature)
}

func resultTuple(sig *types.Signature) *types.Tuple {
	if results := sig.Results(); results != nil {
		return results
	}
	return xtypes.EmptyTuple
}

func funcDeclIndex(env *symbols.Env, p *xast.Package) map[string]*ast.FuncDecl {
	res := make(map[string]*ast.FuncDecl, 32)
	for _, f := range p.AstPkg.Files {
		for _, decl := range f.Decls {
			if decl, ok := decl.(*ast.FuncDecl); ok {
				res[env.InternVar(p.TypPkg, decl.Name.Name)] = decl
			}
		}
	}
	return res
}

func collectParamNames(params []string, decl *ast.FuncDecl) []string {
	for _, p := range decl.Type.Params.List {
		for _, ident := range p.Names {
			params = append(params, ident.Name)
		}
	}
	return params
}
