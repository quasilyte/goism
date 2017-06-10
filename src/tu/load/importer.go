package load

import (
	"errors"
	"go/types"
	"magic_pkg/emacs/lisp"
	"strings"
)

type emacsImporter struct {
	impl types.Importer
}

func (ei *emacsImporter) Import(path string) (*types.Package, error) {
	if !strings.HasPrefix(path, "emacs/") {
		return nil, errors.New("missing `emacs/' package path prefix")
	}

	pkg, err := ei.impl.Import(path)
	if path == "emacs/lisp" && err == nil && lisp.Package == nil {
		return pkg, lisp.InitPackage(pkg)
	}
	return pkg, err
}
