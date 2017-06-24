package generic

import "sexp"

// Coster provides a method to measure Form evaluation complexity.
// Different targets may have different costs for similar operations.
//
// Cost returns a value that approximates computational
// complexity of passed argument.
type Coster interface {
	Cost(sexp.Form) int
}

// BackendCfg describes particular compiler backend.
type BackendCfg struct {
	Name   string
	Coster Coster
}

// Backend incorporates all backend-specific parts.
// Actually is BackendCfg, but fields are readonly.
type Backend struct{ cfg BackendCfg }

// NewBackend creates a new backend driver.
func NewBackend(cfg BackendCfg) *Backend {
	return &Backend{cfg: cfg}
}

// Name returns backend name.
func (b Backend) Name() string { return b.cfg.Name }

// Coster returns backend-specific Coster interface implementation.
func (b Backend) Coster() Coster { return b.cfg.Coster }
