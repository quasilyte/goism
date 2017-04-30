package tu

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"sexp"
	"sizes"
	"strconv"
)

type visitor struct {
	result sexp.Node
	info   *types.Info
}

// Visit implements ast.Visitor interface.
func (v *visitor) Visit(node ast.Node) ast.Visitor {
	switch node := node.(type) {
	case *ast.BlockStmt:
		v.result = &sexp.Block{Nodes: v.visitStmtList(node.List)}

	case *ast.ReturnStmt:
		v.result = &sexp.Return{Results: v.visitExprList(node.Results)}

	case *ast.IfStmt:
		v.result = v.visitIfStmt(node)

	case *ast.BinaryExpr:
		v.result = v.visitBinaryExpr(node)

	case *ast.Ident:
		v.result = sexp.Var{Name: node.Name}

	case *ast.BasicLit:
		v.result = v.visitLiteral(node)

	default:
		panic(fmt.Sprintf("unexpected node: %#v", node))
	}

	return nil
}

func (v *visitor) visit(node ast.Node) sexp.Node {
	v.Visit(node)
	return v.result
}

func (v *visitor) visitLiteral(node *ast.BasicLit) sexp.Node {
	switch node.Kind {
	case token.INT:
		val, err := strconv.ParseInt(node.Value, 10, 64)
		if err != nil {
			panic(err)
		}
		return sexp.Int{Val: val}
	case token.FLOAT:
		val, err := strconv.ParseFloat(node.Value, 64)
		if err != nil {
			panic(err)
		}
		return sexp.Float{Val: val}

	default:
		panic(fmt.Sprintf("unexpected literal: %#v", node))
	}
}

func (v *visitor) visitBinaryExpr(node *ast.BinaryExpr) sexp.Node {
	args := []sexp.Node{
		v.visit(node.X),
		v.visit(node.Y),
	}
	typ := v.info.Types[node].Type.(*types.Basic)

	switch node.Op {
	case token.ADD:
		return &sexp.IntAdd{Args: args, Size: sizes.New(typ)}
	case token.SUB:
		return &sexp.IntSub{Args: args, Size: sizes.New(typ)}
	case token.MUL:
		return &sexp.IntMul{Args: args, Size: sizes.New(typ)}
	case token.QUO:
		return &sexp.IntDiv{Args: args, Size: sizes.New(typ)}
	// #TODO: other arith ops

	case token.EQL:
		return &sexp.IntEq{Args: args}

	default:
		panic(fmt.Sprintf("unexpected op: %v", node.Op))
	}
}

func (v *visitor) visitIfStmt(node *ast.IfStmt) sexp.Node {
	if node.Init != nil {
		panic("If.Init unimplemented")
	}

	test := v.visit(node.Cond)
	then := v.visit(node.Body)
	form := &sexp.If{Test: test, Then: then}
	if node.Else != nil {
		form.Else = v.visit(node.Else)
	}

	return form
}

func (v *visitor) visitExprList(nodes []ast.Expr) []sexp.Node {
	forms := make([]sexp.Node, len(nodes))
	for i, node := range nodes {
		forms[i] = v.visit(node)
	}
	return forms
}

func (v *visitor) visitStmtList(nodes []ast.Stmt) []sexp.Node {
	forms := make([]sexp.Node, len(nodes))
	for i, node := range nodes {
		forms[i] = v.visit(node)
	}
	return forms
}
