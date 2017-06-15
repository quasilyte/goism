package load

import (
	"go/ast"
	"go/types"
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

func collectParamNames(params []string, decl *ast.FuncDecl) []string {
	for _, p := range decl.Type.Params.List {
		for _, ident := range p.Names {
			params = append(params, ident.Name)
		}
	}
	return params
}
