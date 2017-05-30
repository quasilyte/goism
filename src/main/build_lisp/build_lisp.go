package main

import (
	"main/util"
	"os"
	"text/template"
	"time"
)

var Version string

func init() {
	program := &util.ProgramInfo
	program.Description =
		"Prepare Emacs Lisp package for installation."
	program.Name = "goel_build_lisp"
}

func main() {
	tmplData := map[string]interface{}{
		"year":    time.Now().Year(),
		"version": Version,
	}
	tmplSources := []string{
		`lisp/*.el`,
		`lisp/interactive/*el`,
		`lisp/ir/*el`,
		`lisp/runtime/*.el`,
	}
	tmpl := template.New("main")
	for _, src := range tmplSources {
		tmpl = template.Must(tmpl.ParseGlob(src))
	}

	tmpl.ExecuteTemplate(os.Stdout, "main", tmplData)
}
