package tu

import (
	"go/types"
	"lisp"
)

// This importer handles "emacs/lisp" special package import.
type emacsImporter struct {
	impl types.Importer
}

func (ei *emacsImporter) Import(path string) (*types.Package, error) {
	pkg, err := ei.impl.Import(path)
	if path == "emacs/lisp" && err == nil && lisp.Package == nil {
		lisp.InitPackage(pkg)
	}
	return pkg, err
}
