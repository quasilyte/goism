package tu

import (
	"go/types"
)

var emacsPackage *types.Package

type emacsImporter struct {
	impl types.Importer
}

func (ei emacsImporter) Import(path string) (*types.Package, error) {
	// if path == "emacs" {
	// 	return emacsPackage, nil
	// }
	return ei.impl.Import(path)
}

func init() {
	// #TODO: fill emacsPackage.
}
