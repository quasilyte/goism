package main

import (
	"exn"
	"fmt"
	"ir"
	"ir/compiler"
	"ir/export"
	"main/util"
	"opt"
	"regexp"
	"sexp"
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
		"filter": {
			Help: "Regexp to filter 'output=asm' symbols",
		},
	})

	defer func() { util.CheckError(exn.Catch(recover())) }()

	util.CheckError(load.Runtime())
	pkg := loadPackage(util.Argv("pkgPath"), util.Argv("opt") != "false")

	switch util.Argv("output") {
	case "pkg":
		producePackage(pkg)
	case "asm":
		produceAsm(pkg)
	}
}

func loadPackage(pkgPath string, optimize bool) *tu.Package {
	pkg, err := load.Package(util.Argv("pkgPath"))
	util.CheckError(err)
	if optimize {
		opt.OptimizeFuncs(pkg.Funcs)
	}
	return pkg
}

func produceAsm(pkg *tu.Package) {
	cl := compiler.New()

	var filter *regexp.Regexp
	if util.Argv("filter") != "" {
		filter = regexp.MustCompile(`\b` + util.Argv("filter") + `\b`)
	}

	if len(pkg.Vars) > 0 {
		fmt.Println("variables:")
		for _, v := range pkg.Vars {
			if filter == nil || filter.MatchString(v) {
				fmt.Println(" ", v)
			}
		}
		println()
	}

	if len(pkg.Init.Body.Forms) != 0 {
		if filter == nil || filter.MatchString(pkg.Init.Name) {
			fmt.Println("init:")
			dumpFunction(pkg.Init, compileFunc(cl, pkg.Init))
		}
	}

	if len(pkg.Funcs) > 0 {
		fmt.Println("functions:")
		for _, fn := range pkg.Funcs {
			if filter == nil || filter.MatchString(fn.Name) {
				dumpFunction(fn, compileFunc(cl, fn))
			}
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

func compileFunc(cl *compiler.Compiler, fn *sexp.Func) *ir.Object {
	sexpconv.Simplify(fn.Body)
	opt.InlineCalls(fn) // #REFS: 38
	return cl.CompileFunc(fn)
}

func dumpFunction(fn *sexp.Func, obj *ir.Object) {
	fmt.Printf(
		"  fn %s {args=%s max-stack=%d}\n",
		fn.Name, fn.Params, obj.StackUsage,
	)
	fmt.Printf("\tconstants = %s\n", string(obj.ConstVec.Bytes()))
	fmt.Printf("  %s\n", strings.Replace(string(obj.Code), "\n", "\n  ", -1))
}
