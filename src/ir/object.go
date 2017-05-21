package ir

import (
	"dt"
)

type Object struct {
	StackUsage int
	Code       []byte
	ConstVec   *dt.ConstPool
}
