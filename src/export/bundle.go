package export

import (
	"bytecode"
	"bytes"
	"fmt"
)

type Bundle struct {
	prefix string
	buf    bytes.Buffer
}

func NewBuilder(pkgName string) *Bundle {
	buf := bytes.Buffer{}
	fmt.Fprintf(&buf, ";;; Go package `%s'\n", pkgName)
	return &Bundle{
		prefix: "Go-" + pkgName + ".",
		buf:    buf,
	}
}

func (b *Bundle) Build() []byte {
	return b.buf.Bytes()
}

func (b *Bundle) AddFunc(name string, f *bytecode.Func, code []byte) {
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
