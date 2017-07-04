package asm

import (
	"backends/lapc/ir"
	"bytes"
	"strconv"
)

func writeLabel(dst *bytes.Buffer, ins *ir.Instr) {
	dst.Write(ir.EncodingOf(ins.Kind).Name)
	dst.WriteByte(' ')
	dst.WriteString(ins.Meta)
	dst.WriteByte('-')
	dst.WriteString(strconv.FormatUint(uint64(ins.Data), 10))
	dst.WriteByte('\n')
}

func writeOp1(dst *bytes.Buffer, op []byte, data int32) {
	dst.Write(op)
	dst.WriteByte(' ')
	dst.WriteString(strconv.FormatUint(uint64(data), 10))
	dst.WriteByte('\n')
}

func writeOp0(dst *bytes.Buffer, op []byte) {
	dst.Write(op)
	dst.WriteByte('\n')
}
