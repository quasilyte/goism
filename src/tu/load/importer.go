package load

import (
	"go/types"
	"magic_pkg/emacs/lisp"
)

type emacsImporter struct {
	impl types.Importer
}

func (ei *emacsImporter) Import(path string) (*types.Package, error) {
	if err := checkPkgPath(path); err != nil {
		return nil, err
	}

	pkg, err := ei.impl.Import(path)
	if path == "emacs/lisp" && err == nil && lisp.Package == nil {
		return pkg, lisp.InitPackage(pkg)
	}
	return pkg, err
}
