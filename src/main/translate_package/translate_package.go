package main

import (
	"fmt"
	"ir"
	"ir/compiler"
	"ir/export"
	"main/util"
	"opt"
	"strings"
	"tu"
)

var programInfo = util.ProgramInfo{
	Description: "Translate single Go package into IR format.",
}

func main() {
	util.ParseArgv(&programInfo, util.ArgvSchema{
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

	pkg, err := tu.TranslatePackage(util.Argv("pkgPath"))
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
	cl := compiler.New()

	if len(pkg.Vars) > 0 {
		fmt.Println("variables:")
	}
	for _, v := range pkg.Vars {
		fmt.Println(" ", v)
	}

	fmt.Println("init:")
	obj := cl.CompileFunc(pkg.Init)
	dumpFunction(pkg.Init, obj)

	if len(pkg.Funcs) > 0 {
		fmt.Println("functions:")
	}
	for _, fn := range pkg.Funcs {
		obj := cl.CompileFunc(fn)
		dumpFunction(fn, obj)
	}
}

func producePackage(pkg *tu.Package) {
	cl := compiler.New()

	output := export.NewBuilder(pkg)

	output.AddVars(pkg.Vars)
	for _, fn := range pkg.Funcs {
		obj := cl.CompileFunc(fn)
		output.AddFunc(fn, obj)
	}

	fmt.Print(string(output.Build()))
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
		opt.ReduceStrength(fn.Body)
	}
}
