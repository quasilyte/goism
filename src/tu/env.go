package tu

type Env struct {
	pkgName string
	symbols map[string]string
	funcs   map[string]*Func
}

func NewEnv(pkgName string) *Env {
	return &Env{
		pkgName: pkgName,
		symbols: make(map[string]string),
		funcs:   make(map[string]*Func),
	}
}

func (env *Env) PkgName() string {
	return env.pkgName
}

func (env *Env) SetFuncs(funcs []*Func) {
	for _, fn := range funcs {
		env.funcs[fn.Typ.Name()] = fn
	}
}

func (env *Env) Func(name string) *Func {
	return env.funcs[name]
}

func (env *Env) ContainsVar(name string) bool {
	_, ok := env.symbols[name]
	return ok
}

func (env *Env) InternVar(name string) string {
	if sym := env.symbols[name]; sym != "" {
		return sym
	}
	sym := varName(env.pkgName, name)
	env.symbols[name] = sym
	return sym
}
