package load

import (
	"go/ast"
	"go/types"
	"sexp"
)

func convertInitializers(u *unit) {
	body := make([]sexp.Form, 0, 16)
	vars := make([]string, 0, 8)

	blankIdent := &ast.Ident{Name: "_"}
	for _, init := range u.ti.InitOrder {
		idents := make([]*ast.Ident, len(init.Lhs))
		for i, v := range init.Lhs {
			if v.Name() == "_" {
				idents[i] = blankIdent
			} else {
				idents[i] = &ast.Ident{Name: v.Name()}
				u.ti.Uses[idents[i]] = v
				vars = append(vars, u.env.InternVar(v.Name()))
			}
		}

		body = append(body, u.conv.VarInit(idents, init.Rhs))

		// Clear information that was added to type info above.
		for _, ident := range idents {
			delete(u.ti.Uses, ident)
		}
	}

	// InitOrder misses entries for variables without explicit
	// initializers. They are collected here.
	for _, name := range u.topScope.Names() {
		if v, ok := u.topScope.Lookup(name).(*types.Var); ok {
			if u.env.ContainsVar(v.Name()) {
				continue
			}
			sym := u.env.InternVar(v.Name())
			vars = append(vars, sym)
			body = append(body, u.conv.VarZeroInit(sym, v.Type()))
		}
	}

	if len(body) != 0 {
		body = append(body, &sexp.Return{})
	}

	u.init = &sexp.Func{
		Name: "init",
		Body: &sexp.Block{Forms: body},
	}
	u.vars = vars
}
