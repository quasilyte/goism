package lisp

type Env struct {
	pkgName string
	symbols map[string]string
}

func NewEnv(pkgName string) *Env {
	return &Env{
		pkgName: pkgName,
		symbols: make(map[string]string),
	}
}

func (env *Env) PkgName() string {
	return env.pkgName
}

func (env *Env) Contains(name string) bool {
	_, ok := env.symbols[name]
	return ok
}

func (env *Env) Intern(name string) string {
	if sym := env.symbols[name]; sym != "" {
		return sym
	}
	sym := varName(env.pkgName, name)
	env.symbols[name] = sym
	return sym
}
