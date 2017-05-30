package tu

import (
	"go/types"
	"sexp"
)

func convertInitializers(u *unit) {
	body := make([]sexp.Form, 0, 16)
	vars := make([]string, 0, 8)

	for _, init := range u.ti.InitOrder {
		names := make([]string, len(init.Lhs))
		for i, v := range init.Lhs {
			if v.Name() == "_" {
				names[i] = "_"
			} else {
				names[i] = u.env.Intern(v.Name())
				vars = append(vars, names[i])
			}
		}

		body = append(body, u.conv.VarInit(names, init.Rhs))
	}

	for _, name := range u.topScope.Names() {
		if v, ok := u.topScope.Lookup(name).(*types.Var); ok {
			if u.env.Contains(v.Name()) {
				continue
			}
			name := u.env.Intern(v.Name())
			vars = append(vars, name)
			body = append(body, u.conv.VarZeroInit(name, v.Type()))
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
