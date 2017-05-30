package tu

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
				vars = append(vars, u.env.Intern(v.Name()))
			}
		}

		body = append(body, u.conv.VarInit(idents, init.Rhs))
	}

	for _, name := range u.topScope.Names() {
		if v, ok := u.topScope.Lookup(name).(*types.Var); ok {
			if u.env.Contains(v.Name()) {
				continue
			}
			sym := u.env.Intern(v.Name())
			vars = append(vars, sym)
			body = append(body, u.conv.VarZeroInit(sym, v.Type()))
		}
	}

	if len(body) != 0 {
		body = append(body, &sexp.Return{})
	}

	u.init = &Func{
		Name: "init",
		Body: &sexp.Block{Forms: body},
	}
	u.vars = vars
}
