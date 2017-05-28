package tu

import (
	"bytes"
	"go/ast"
	"go/token"
	"path/filepath"
	"sexp"
	"sexpconv"
)

// Package contains information about parsed code.
type Package struct {
	Name string

	Funcs []*Func

	// Vars are sorted in order that should be used
	// during initialization.
	Vars []string
	Init *Func

	Comment string
}

// Func is a Sexp function.
type Func struct {
	Name      string
	Body      *sexp.Block
	Params    []string
	Variadic  bool
	DocString string
}

// TranslatePackage converts Go package into Sexp package.
//
// It parses and typechecks specified package,
// then converts generated objects into our format.
func TranslatePackage(pkgPath string) (pkg *Package, err error) {
	fSet := token.NewFileSet()

	parsedPkg, err := parsePackage(fSet, pkgPath)
	if err != nil {
		return nil, err
	}

	checkedPkg, err := typecheckPackage(fSet, parsedPkg)
	if err != nil {
		return nil, err
	}

	defer func() {
		switch panicArg := recover().(type) {
		case nil:
			return
		case sexpconv.Error:
			pkg = nil
			err = panicArg
			return
		default:
			panic(panicArg)
		}
	}()
	pkgComment := packageComment(parsedPkg.Files)
	return translatePackage(checkedPkg, pkgComment), nil
}

func packageComment(files map[string]*ast.File) string {
	var buf bytes.Buffer
	buf.WriteString(";; ") // To avoid expensive prepend in the end.

	for name, file := range files {
		if file.Doc != nil {
			buf.WriteString("\t<")
			buf.WriteString(filepath.Base(name))
			buf.WriteString(">\n")
			buf.WriteString(file.Doc.Text())
		}
	}

	if buf.Len() == len(";; ") {
		return ""
	}

	// Remove trailing newline.
	buf.Truncate(buf.Len() - 1)

	// Properly format comment text.
	comment := bytes.Replace(buf.Bytes(), []byte(`"`), []byte(`\"`), -1)
	comment = bytes.Replace(comment, []byte("\n"), []byte("\n;; "), -1)

	return string(comment)
}
