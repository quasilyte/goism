package export

import (
	"bytecode"
	"bytes"
	"fmt"
)

type Builder struct {
	prefix string
	buf    bytes.Buffer
}

func NewBuilder(pkgName string) *Builder {
	buf := bytes.Buffer{}
	fmt.Fprintf(&buf, ";;; Go package `%s'\n", pkgName)
	return &Builder{
		prefix: "Go-" + pkgName + ".",
		buf:    buf,
	}
}

func (b *Builder) Build() []byte {
	return b.buf.Bytes()
}

func (b *Builder) AddFunc(name string, f *bytecode.Func, code []byte) {
	qualName := b.prefix + name
	escapedCode := escapeBytecode(code)
	constVec := f.ConstPool.String()

	fmt.Fprintf(
		&b.buf,
		"(defalias '%s #[%d \"%s\" %s %d])\n",
		qualName, f.ArgsDesc, escapedCode, constVec, f.StackUsage,
	)
}

func escapeBytecode(code []byte) string {
	buf := bytes.Buffer{}
	for _, c := range code {
		buf.Write(escapedOctals[c])
	}
	return buf.String()
}
