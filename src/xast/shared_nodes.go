package xast

import (
	"go/ast"
	"go/token"
	"lisp"
)

var multiRetIdent = initMultiRetIdent()

var (
	zeroStr   = &ast.BasicLit{Kind: token.STRING, Value: ""}
	zeroInt   = &ast.BasicLit{Kind: token.INT, Value: "0"}
	zeroFloat = &ast.BasicLit{Kind: token.FLOAT, Value: "0.0"}
	zeroBool  = &ast.Ident{Name: "false"}
)

// Used when node removal is needed.
var (
	EmptyStmt = &ast.EmptyStmt{}
	EmptyDecl = &ast.GenDecl{Lparen: -1, Rparen: -1, Tok: token.VAR}
)

func initMultiRetIdent() []*ast.Ident {
	res := make([]*ast.Ident, len(lisp.RetVars))
	for i := 1; i < len(lisp.RetVars); i++ {
		res[i] = &ast.Ident{Name: lisp.RetVars[i]}
	}
	return res
}
