package tu

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"sexp"
	"strconv"
	"unicode/utf8"
)

type visitor struct {
	result       sexp.Node
	info         *types.Info
	globalValues map[string]sexp.Node
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
		val := v.globalValues[node.Name]
		if val == nil {
			v.result = sexp.Var{Name: node.Name}
		} else {
			v.result = val
		}

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

	case token.STRING:
		val := node.Value[1 : len(node.Value)-1]
		return sexp.String{Val: val}

	case token.CHAR:
		val, _ := utf8.DecodeRuneInString(node.Value[1:])
		return sexp.Char{Val: val}

	default:
		panic(fmt.Sprintf("unexpected literal: %#v", node))
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

func (v *visitor) visitBinaryExpr(node *ast.BinaryExpr) sexp.Node {
	args := []sexp.Node{
		v.visit(node.X),
		v.visit(node.Y),
	}

	// #FIXME: size information is unused.
	kind := mapKind(v.info.Types[node].Type.(*types.Basic))

	switch kind.tag + int64(node.Op)<<32 {
	case int64(token.ADD)<<32 + kindInt:
		return &sexp.IntAdd{Args: args}
	case int64(token.ADD)<<32 + kindFloat:
		return &sexp.FloatAdd{Args: args}
	case int64(token.ADD)<<32 + kindString:
		return &sexp.Concat{Args: args}

	case int64(token.SUB)<<32 + kindInt:
		return &sexp.IntSub{Args: args}
	case int64(token.SUB)<<32 + kindFloat:
		return &sexp.FloatSub{Args: args}

	case int64(token.MUL)<<32 + kindInt:
		return &sexp.IntMul{Args: args}
	case int64(token.MUL)<<32 + kindFloat:
		return &sexp.FloatMul{Args: args}

	case int64(token.QUO)<<32 + kindInt:
		return &sexp.IntDiv{Args: args}
	case int64(token.QUO)<<32 + kindFloat:
		return &sexp.FloatDiv{Args: args}

	case int64(token.EQL)<<32 + kindInt:
		return &sexp.IntEq{Args: args}
	case int64(token.EQL)<<32 + kindFloat:
		return &sexp.FloatEq{Args: args}
	case int64(token.EQL)<<32 + kindString:
		return &sexp.StringEq{Args: args}

	default:
		panic(fmt.Sprintf("unexpected op: %v", node.Op))
	}
}
