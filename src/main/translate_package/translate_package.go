package main

import (
	"fmt"
	"ir"
	"ir/compiler"
	"ir/export"
	"main/util"
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
	})

	pkg, err := tu.TranslatePackage(util.Argv("pkgPath"))
	util.CheckError(err)

	switch util.Argv("output") {
	case "pkg":
		producePackage(pkg)
	case "asm":
		produceAsm(pkg)
	}
}

func dumpFunction(f *ir.Func) {
	fmt.Printf(
		"  fn %s {args=%x max-stack=%d}\n",
		f.Name, f.ArgsDesc, f.StackUsage,
	)
	fmt.Printf("constants = %s\n", string(f.ConstVec.Bytes()))
	fmt.Printf("  %s\n", strings.Replace(string(f.Body), "\n", "\n  ", -1))
}

func produceAsm(pkg *tu.Package) {
	cl := compiler.New()

	fmt.Printf("Package %s:\n\n", pkg.Name)

	for i := range pkg.Funcs {
		f := cl.CompileFunc(pkg.Funcs[i])
		dumpFunction(f)
	}
}

func producePackage(pkg *tu.Package) {
	cl := compiler.New()

	output := export.NewBuilder(pkg.Name)

	for i := range pkg.Funcs {
		f := cl.CompileFunc(pkg.Funcs[i])
		output.AddFunc(f)
	}

	fmt.Print(string(output.Build()))
}
