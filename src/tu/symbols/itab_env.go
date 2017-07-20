package symbols

import (
	"go/types"
)

// ItabEnv used to store interface dynamic type info.
type ItabEnv struct {
	masterPkg *types.Package

	vals        map[itabKey]string
	masterItabs []Itab
}

type typeInfo struct {
	name string
	pkg  *types.Package
}

type itabKey struct {
	impl  *types.Named
	iface *types.Named
}

// Itab contains information about interface table variable.
type Itab struct {
	Name     string // Symbol name
	ImplName string // Implementation type name
	Iface    *types.Interface
}

func NewItabEnv(masterPkg *types.Package) *ItabEnv {
	return &ItabEnv{
		masterPkg: masterPkg,
		vals:      make(map[itabKey]string, 32),
	}
}

// Intern returns itab variable name.
func (env *ItabEnv) Intern(implTyp, ifaceTyp *types.Named) string {
	key := itabKey{impl: implTyp, iface: ifaceTyp}
	if val := env.vals[key]; val != "" {
		return val
	}
	implObj, ifaceObj := implTyp.Obj(), ifaceTyp.Obj()
	implStr := implObj.Name()
	ifaceStr := ifaceObj.Pkg().Name() + "." + ifaceObj.Name()
	name := "%itab/" + implStr + "/" + ifaceStr
	// #FIXME: should have full package path here instead of Pkg().Name().
	sym := ManglePriv(implObj.Pkg().Name(), name)
	env.vals[key] = sym
	if implObj.Pkg() == env.masterPkg {
		env.masterItabs = append(env.masterItabs, Itab{
			Name:     sym,
			Iface:    ifaceTyp.Underlying().(*types.Interface),
			ImplName: implObj.Name(),
		})
	}
	return sym
}

func (env *ItabEnv) GetMasterItabs() []Itab {
	return env.masterItabs
}
