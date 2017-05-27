package main

import (
	"fmt"
	"go/printer"
	"ir/compiler"
	"main/util"
	"os"
	"tu"
)

func init() {
	program := &util.ProgramInfo
	program.Description =
		"Translate single Go package into IR format."
	program.Name = "goel-translate-package"
}

/*
type simplifier struct {
	*types.Info
	retVars []*ast.Ident
}

func NewSimplifier(info *types.Info) *simplifier {
	retVars := make([]*ast.Ident, len(lisp.RetVars))
	for i := 1; i < len(lisp.RetVars); i++ {
		retVars[i] = &ast.Ident{Name: lisp.RetVars[i]}
	}
	return &simplifier{
		Info:    info,
		retVars: retVars,
	}
}

func (sim *simplifier) multiValueList(first ast.Expr, count int) []ast.Expr {
	nodes := make([]ast.Expr, count)
	nodes[0] = first
	for i := 1; i < count; i++ {
		nodes[i] = sim.retVars[i]
	}
	return nodes
}

func (sim *simplifier) Simplify(node ast.Node) ast.Node {
	e := func(node ast.Expr) ast.Expr {
		return xast.RewriteExpr(node, sim.Simplify)
	}

	switch node := node.(type) {
	case *ast.BinaryExpr:
		switch node.Op {
		case token.SHR:
			x, y := e(node.X), e(node.Y)
			return xast.Shl(x, xast.Neg(y))

		case token.NEQ:
			x, y := e(node.X), e(node.Y)
			return xast.Not(xast.Eq(x, y))
		}

	case *ast.GenDecl:
		if node.Tok == token.CONST {
			// Constants are not used to generate IR code.
			return xast.EmptyDecl
		}

	case *ast.AssignStmt:
		if node.Tok == token.ASSIGN {
			if len(node.Lhs) != len(node.Rhs) {
				node.Rhs = sim.multiValueList(node.Rhs[0], len(node.Lhs))
				return node
			}
			return nil
		}
		// Split DEFINE and ASSIGN.
		var nodes []ast.Stmt
		for i, lhs := range node.Lhs {
			if ident, ok := lhs.(*ast.Ident); ok && sim.Defs[ident] != nil {
				nodes = append(nodes, xast.NewBind(ident, node.Rhs[i]))
			} else {
				nodes = append(nodes, xast.NewRebind(lhs, node.Rhs[i]))
			}
		}
		return &xast.StmtList{List: nodes}

	case *ast.ValueSpec:
		if len(node.Names) == len(node.Values) || len(node.Names) == 0 {
			return nil
		}
		if len(node.Values) == 0 {
			// Insert explicit zero value initializers.
			node.Values = make([]ast.Expr, len(node.Names))
			zv := xast.ZeroValue(sim.Defs[node.Names[0]].Type())
			for i := range node.Values {
				node.Values[i] = zv
			}
			return node
		}
		// Rewrite multi value assignments.
		node.Values = sim.multiValueList(node.Values[0], len(node.Names))
		return node
	}

	return nil
}
*/

func main() {
	util.ParseArgv(util.ArgvSchema{
		"pkgPath": {
			Help: "Path to Go package to be translated",
			Req:  true,
		},
		"output": {
			Help: "Produced output: {pkg|asm|ast}",
			Init: "pkg",
			Enum: true,
		},
		"opt": {
			Help: "Set to false to get unoptimized output",
			Init: "true",
		},
	})

	// Before compiling, transformations needed:
	// - replace ops like SHR, NEQ...
	// - replace multi bind with explicit Go--ret-N var refs
	// - rewrite "x, y := 10, 20" to proper bind and assign.

	// types in compiler needed to:
	// - to correctly drop scope bindings
	//   ->
	// - determine void functions
	//   -> keep functions info nearby. It does not get spoiled with AST rewrite
	// - resolve binary ops like "+" and "<"
	//   -> store OpKinds
	// - resolve literals
	//   -> collect constants into separate map and use "Value" as a key

	// 1: fill Env info and simplify AST
	// 2: optimize
	// 3: replace unsupported ops with equivalents
	// 4: compile

	pkg, err := tu.TranslatePackage(util.Argv("pkgPath"))
	util.CheckError(err)

	if util.Argv("opt") != "false" {
		optimizePackage(pkg)
	}

	switch util.Argv("output") {
	case "pkg":
		// producePackage(pkg)
	case "asm":
		produceAsm(pkg)
	case "ast":
		produceAst(pkg)
	}
}

func optimizePackage(pkg *tu.Package) {
	// for _, fn := range pkg.Funcs {
	// 	opt.RemoveDeadCode(fn.Body)
	// 	opt.ReduceStrength(fn.Body)
	// }
}

func produceAst(pkg *tu.Package) {
	for _, fn := range pkg.Funcs {
		fmt.Printf("%s := ", fn.Decl.Name.Name)
		printer.Fprint(os.Stdout, pkg.FileSet, fn.Decl.Type)
		printer.Fprint(os.Stdout, pkg.FileSet, fn.Root)
		println()
	}
}

func produceAsm(pkg *tu.Package) {
	cl := compiler.New(pkg)

	for _, fn := range pkg.Funcs {
		obj := cl.CompileFunc(fn)
		// fmt.Printf("%#v\n", obj)
		fmt.Printf("\tconstants = %s\n", string(obj.ConstVec.Bytes()))
		fmt.Println(string(obj.Code))
	}

	/*
		if len(pkg.Vars) > 0 {
			fmt.Println("variables:")
			for _, v := range pkg.Vars {
				fmt.Println(" ", v)
			}
			println()
		}

		if len(pkg.Init.Body.Forms) != 0 {
			fmt.Println("init:")
			dumpFunction(pkg.Init, compileFunc(cl, pkg.Init))
		}

		if len(pkg.Funcs) > 0 {
			fmt.Println("functions:")
			for _, fn := range pkg.Funcs {
				dumpFunction(fn, compileFunc(cl, fn))
			}
		}
	*/
}

/*
func dumpFunction(fn *tu.Func, obj *ir.Object) {
	fmt.Printf(
		"  fn %s {args=%s max-stack=%d}\n",
		fn.Name, fn.Params, obj.StackUsage,
	)
	fmt.Printf("\tconstants = %s\n", string(obj.ConstVec.Bytes()))
	fmt.Printf("  %s\n", strings.Replace(string(obj.Code), "\n", "\n  ", -1))
}
*/

/*
func produceAsm(pkg *tu.Package) {
	cl := compiler.New()

	if len(pkg.Vars) > 0 {
		fmt.Println("variables:")
		for _, v := range pkg.Vars {
			fmt.Println(" ", v)
		}
		println()
	}

	if len(pkg.Init.Body.Forms) != 0 {
		fmt.Println("init:")
		dumpFunction(pkg.Init, compileFunc(cl, pkg.Init))
	}

	if len(pkg.Funcs) > 0 {
		fmt.Println("functions:")
		for _, fn := range pkg.Funcs {
			dumpFunction(fn, compileFunc(cl, fn))
		}
	}
}

func producePackage(pkg *tu.Package) {
	cl := compiler.New()

	output := export.NewBuilder(pkg)

	if len(pkg.Vars) != 0 {
		output.AddVars(pkg.Vars)
	}

	for _, fn := range pkg.Funcs {
		output.AddFunc(fn, compileFunc(cl, fn))
	}

	if len(pkg.Init.Body.Forms) != 0 {
		output.AddExpr(compileFunc(cl, pkg.Init))
	}

	fmt.Print(string(output.Build()))
}

func compileFunc(cl *compiler.Compiler, fn *tu.Func) *ir.Object {
	sexpconv.Simplify(fn.Body)
	return cl.CompileFunc(fn)
}




*/
