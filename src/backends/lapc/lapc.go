package lapc

import (
	"backends/gen"
)

// NewBackend returns lapc compiler backend.
func NewBackend() *gen.Backend {
	return gen.NewBackend(gen.BackendCfg{
		Name:   "lapc",
	})
}
