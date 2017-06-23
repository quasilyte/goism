package elapc

import (
	"dt"
)

// Object is a compiled IR unit.
type Object struct {
	StackUsage int
	Code       []byte
	ConstVec   *dt.ConstPool
}
