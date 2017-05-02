package sexpconv

import (
	"go/ast"
	"go/types"
	"sexp"
)

func Node(info *types.Info, node ast.Node) sexp.Form {
	// #FIXME: is this method needed?
	// If yes, implement it. Otherwise remove it.
	panic("unimplemented")
}
