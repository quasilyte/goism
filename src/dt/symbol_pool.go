package dt

// SymbolPool collects a set of symbols that are
// identified by their ID.
//
// It can contain symbols with same name but different
// ID. This property makes it suitable to handle
// lexical scoping during compilation.
type SymbolPool struct {
	symbols []string
	scope   []symbolPoolItem
}

type symbolPoolItem struct {
	index int
	name  string
}

// Insert new symbol with specified name.
// Returns symbol unique ID.
func (sp *SymbolPool) Insert(name string) int {
	index := len(sp.symbols)
	sp.symbols = append(sp.symbols, name)
	sp.scope = append(sp.scope, symbolPoolItem{
		index: index,
		name:  name,
	})
	return index
}

// Find lookups symbol by name.
// If there are multiple symbols with same name,
// newer one is selected.
func (sp *SymbolPool) Find(name string) int {
	for i := len(sp.scope) - 1; i >= 0; i-- {
		item := sp.scope[i]
		if item.name == name {
			return item.index
		}
	}
	return -1
}

// Drop removes last N inserted elements from lookup table.
// It does not remove any collected symbol.
func (sp *SymbolPool) Drop(n int) {
	sp.scope = sp.scope[:len(sp.scope)-n]
}

// Symbols returns all collected symbols as a slice.
// Slice can be indexed by ID to retrive symbol name.
func (sp *SymbolPool) Symbols() []string {
	return sp.symbols
}
