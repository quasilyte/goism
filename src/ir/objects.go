package ir

import (
	"dt"
)

type Func struct {
	Name       string
	ArgsDesc   uint32
	StackUsage int
	Body       []byte
	ConstVec   *dt.ConstPool
	DocString  string
}
