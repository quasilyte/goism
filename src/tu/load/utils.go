package load

import (
	"go/ast"
	"go/types"
	"strings"
	"xtypes"

	"github.com/pkg/errors"
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

func checkPkgPath(pkgPath string) error {
	// Only "emacs/" prefix check is mandatory,
	// but in order to provide better error message
	// additional checks are performed.
	if len(pkgPath) <= 2 {
		return errors.New("invalid package path")
	}
	if pkgPath[0] == '/' {
		return errors.New("absolute paths are not supported")
	}
	if pkgPath[0] == '.' && pkgPath[1] == '/' {
		return errors.New("relative paths are not supported")
	}
	if !strings.HasPrefix(pkgPath, "emacs/") {
		return errors.New("missing `emacs/' package path prefix")
	}
	return nil
}
