package xast

import (
	"fmt"
	"go/ast"
	"go/types"
)

func ZeroValue(typ types.Type) ast.Expr {
	switch typ := typ.(type) {
	case *types.Basic:
		info := typ.Info()
		if info&types.IsFloat != 0 {
			return zeroFloat
		}
		if info&types.IsInteger != 0 {
			return zeroInt
		}
		if typ.Kind() == types.String {
			return zeroStr
		}
		if typ.Kind() == types.Bool {
			return zeroBool
		}
	}

	panic(fmt.Sprintf("can not provide zero value for %#v", typ))
}
