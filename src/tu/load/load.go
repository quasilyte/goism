package load

import (
	"bytes"
	"go/ast"
	"go/build"
	"magic_pkg/emacs/rt"
	"opt"
	"path/filepath"
	"tu"
)

// Package converts Go package into Sexp package.
//
// It parses and typechecks specified package,
// then converts generated objects into our format.
func Package(pkgPath string) (*tu.Package, error) {
	u, err := translateUnit(pkgPath)
	if err != nil {
		return nil, err
	}
	comment := packageComment(u.astPkg.Files)
	return newPackage(u, comment), nil
}

// Runtime loads code package that implements goism runtime.
// Package is searched at "$GOPATH/src/emacs/rt".
func Runtime() error {
	u, err := translateUnit(build.Default.GOPATH + "/src/emacs/rt")
	if err != nil {
		return err
	}
	rt.InitPackage(u.typesPkg)
	rt.InitFuncs(u.env)
	opt.OptimizeFuncs(u.funcs)
	return nil
}

func newPackage(u *unit, comment string) *tu.Package {
	return &tu.Package{
		Name:    u.astPkg.Name,
		Vars:    u.vars,
		Funcs:   u.funcs,
		Init:    u.init,
		Comment: comment,
	}
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
