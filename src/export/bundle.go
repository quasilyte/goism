package export

import (
	"bytes"
	"fmt"
	"ir"
)

type Bundle struct {
	prefix string
	buf    bytes.Buffer
}

const pkgHeader = `;;; -*- lexical-binding: t -*-
;; THIS CODE IS GENERATED, AVOID MANUAL EDITING!
;; Go package %s:
`

func NewBuilder(pkgName string) *Bundle {
	buf := bytes.Buffer{}
	fmt.Fprintf(&buf, pkgHeader, pkgName)

	return &Bundle{
		prefix: "Go-" + pkgName + ".",
		buf:    buf,
	}
}

func (b *Bundle) Build() []byte {
	return b.buf.Bytes()
}

func (b *Bundle) AddFunc(name string, f *ir.Func) {
	qualName := b.prefix + name
	escapedCode := escapeBytecode(f.Body)
	constVec := f.ConstVec.String()

	fmt.Fprintf(
		&b.buf,
		"(defalias '%s #[%d \"%s\" %s %d \"%s\"])\n",
		qualName, f.ArgsDesc, escapedCode, constVec, f.StackUsage, f.DocString,
	)
}

func escapeBytecode(code []byte) string {
	buf := bytes.Buffer{}
	for _, c := range code {
		buf.Write(escapedOctals[c])
	}
	return buf.String()
}
