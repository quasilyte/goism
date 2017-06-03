package export

import (
	"ir"
	"sexp"
	"tu"
)

// Builder allows to create exportable package.
// This object is not reusable.
type Builder struct {
	w writer
}

// NewBuilder returns fresh export package builder.
func NewBuilder(pkg *tu.Package) *Builder {
	b := &Builder{}
	w := &b.w

	w.WriteByte('(') // Open list (closed in Build method)

	// Write mandatory header.
	w.WriteSymbol(pkg.Name)
	w.WriteString(pkg.Comment)

	return b
}

// Build finalizes package being built.
// Package bytes returned.
// It is illegal to call Build method twice one the same builder.
func (b *Builder) Build() []byte {
	b.w.WriteByte(')') // Close list
	return b.w.Bytes()
}

// AddFunc pushes function definition into package.
func (b *Builder) AddFunc(fn *sexp.Func, obj *ir.Object) {
	w := &b.w

	w.WriteSymbol("fn")
	w.WriteSymbol(fn.Name)
	w.WriteInt(argsDescriptor(fn))
	w.Write(obj.ConstVec.Bytes())
	w.WriteInt(obj.StackUsage)
	w.WriteString(docString(fn))
	w.Write(obj.Code)

	w.WriteSymbol("end")
}

func (b *Builder) AddExpr(obj *ir.Object) {
	w := &b.w

	w.WriteSymbol("expr")
	w.Write(obj.ConstVec.Bytes())
	w.WriteInt(obj.StackUsage)
	w.Write(obj.Code)

	w.WriteSymbol("end")
}

func (b *Builder) AddVars(names []string) {
	w := &b.w

	w.WriteSymbol("vars")
	for _, name := range names {
		w.WriteSymbol(name)
	}

	w.WriteSymbol("end")
}
