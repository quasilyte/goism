package main

import (
	"exn"
	"fmt"
	"ir"
	"ir/compiler"
	"ir/export"
	"main/util"
	"opt"
	"sexpconv"
	"strings"
	"tu"
	"tu/load"
)

func init() {
	program := &util.ProgramInfo
	program.Description =
		"Translate single Go package into IR format."
	program.Name = "goel_translate_package"
}

func main() {
	util.ParseArgv(util.ArgvSchema{
		"pkgPath": {
			Help: "Path to Go package to be translated",
			Req:  true,
		},
		"output": {
			Help: "Produced output: {pkg|asm}",
			Init: "pkg",
			Enum: true,
		},
		"opt": {
			Help: "Set to false to get unoptimized output",
			Init: "true",
		},
	})

	defer func() { util.CheckError(exn.Catch(recover())) }()

	pkg, err := load.Package(util.Argv("pkgPath"))
	util.CheckError(err)

	if util.Argv("opt") != "false" {
		optimizePackage(pkg)
	}

	switch util.Argv("output") {
	case "pkg":
		producePackage(pkg)
	case "asm":
		produceAsm(pkg)
	}
}

func produceAsm(pkg *tu.Package) {
	cl := compiler.New(pkg.Env)

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
	cl := compiler.New(pkg.Env)

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

func dumpFunction(fn *tu.Func, obj *ir.Object) {
	fmt.Printf(
		"  fn %s {args=%s max-stack=%d}\n",
		fn.Name, fn.Params, obj.StackUsage,
	)
	fmt.Printf("\tconstants = %s\n", string(obj.ConstVec.Bytes()))
	fmt.Printf("  %s\n", strings.Replace(string(obj.Code), "\n", "\n  ", -1))
}

func optimizePackage(pkg *tu.Package) {
	for _, fn := range pkg.Funcs {
		opt.RemoveDeadCode(fn.Body)
		opt.InlineCalls(pkg.Env, fn.Body)
		opt.ReduceStrength(fn.Body)
	}
}
