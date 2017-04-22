package sexp

/*
import (
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"strconv"
)

var emacsPackage *types.Package

var typeChecker = types.Config{
	Error: func(err error) {
		fmt.Println(err)
	},
	Importer: emacsImporter{importer.Default()},
}

type emacsImporter struct {
	impl types.Importer
}

func (ei emacsImporter) Import(path string) (*types.Package, error) {
	if path == "emacs" {
		return emacsPackage, nil
	}
	return ei.impl.Import(path)
}

func init() {
	// #TODO: fill emacsPackage.
}

func Parse(input string) (Node, error) {
	fSet := token.NewFileSet()
	fAst, err := parser.ParseFile(fSet, "${FILENAME}", input, 0)
	if err != nil {
		return nil, err
	}
	info := types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
	}
	_, err = typeChecker.Check("${PATH}", fSet, []*ast.File{fAst}, &info)
	if err != nil {
		return nil, err
	}
	visitor := &goAstVisitor{info: &info}

	fnDecl := fAst.Decls[0].(*ast.FuncDecl)

	ast.Walk(visitor, fnDecl.Body)
	return visitor.res, nil
}

type goAstVisitor struct {
	info *types.Info
	res  Node
}

func visit(v *goAstVisitor, node ast.Node) Node {
	nestedVisitor := &goAstVisitor{info: v.info}
	ast.Walk(nestedVisitor, node)
	return nestedVisitor.res
}

func (v *goAstVisitor) Visit(node ast.Node) ast.Visitor {
	switch node := node.(type) {
	case *ast.BlockStmt:
		println("BlockStmt")
		res := &Block{make([]Node, len(node.List))}
		for i := range node.List {
			res.Nodes[i] = visit(v, node.List[i])
		}
		v.res = res

	case *ast.ReturnStmt:
		println("ReturnStmt")
		// #FIXME: handle multiple value return
		v.res = &Return{visit(v, node.Results[0])}

	case *ast.BinaryExpr:
		println("BinaryExpr")
		var opTypes = map[token.Token]VariadicOpType{
			token.ADD: OpAdd,
			token.MUL: OpMul,
		}

		v.res = &VariadicOp{
			Type: opTypes[node.Op],
			Args: []Node{
				visit(v, node.X),
				visit(v, node.Y),
			},
		}

	case *ast.Ident:
		println("Ident")
		v.res = &Var{node.Name}

	case *ast.BasicLit:
		println("BasicLit")
		switch node.Kind {
		case token.INT:
			val, _ := strconv.ParseInt(node.Value, 10, 0)
			v.res = &Int{val}

		default:
			fmt.Printf("UNKNOWN LIT %#v\n", node)
			return nil
		}

	default:
		println("default")
		fmt.Printf("UNKNOWN NODE %#v\n", node)
		return nil
	}

	return nil
}
*/
