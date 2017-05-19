package export

import (
	"ir"
)

// Builder allows to create exportable package.
// This object is not reusable.
type Builder struct {
	w writer
}

// NewBuilder returns fresh export package builder.
func NewBuilder(name string) *Builder {
	b := &Builder{}
	b.w.WriteByte('(')
	b.w.WriteSymbol(name)
	return b
}

// Build finalizes package being built.
// Package bytes returned.
// It is illegal to call Build method twice one the same builder.
func (b *Builder) Build() []byte {
	b.w.WriteByte(')')
	return b.w.Bytes()
}

// AddFunc pushes function definition into package.
func (b *Builder) AddFunc(f *ir.Func) {
	w := &b.w

	w.WriteSymbol("fn")
	w.WriteSymbol(f.Name)
	w.WriteInt(int(f.ArgsDesc))
	w.Write(f.ConstVec.Bytes())
	w.WriteInt(f.StackUsage)
	w.WriteString(f.DocString)
	w.Write(f.Body)

	w.WriteSymbol("end")
}
