package load

import (
	"go/ast"
	"go/types"
)

func declSignature(ti *types.Info, decl *ast.FuncDecl) *types.Signature {
	return ti.Defs[decl.Name].Type().(*types.Signature)
}

func resultTuple(sig *types.Signature) *types.Tuple {
	if results := sig.Results(); results != nil {
		return results
	}
	return emptyTuple
}

// Collect flat list of param names.
func declParamNames(decl *ast.FuncDecl) []string {
	params := decl.Type.Params
	res := make([]string, 0, params.NumFields())
	for _, p := range params.List {
		for _, ident := range p.Names {
			res = append(res, ident.Name)
		}
	}
	return res
}
