package gen

// BackendCfg describes particular compiler backend.
type BackendCfg struct {
	Name string
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
