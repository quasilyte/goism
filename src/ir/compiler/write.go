package compiler

import (
	"bytes"
	"ir"
	"strconv"
)

func writeLabel(dst *bytes.Buffer, op []byte, label label) {
	dst.Write(op)
	dst.WriteByte(' ')
	dst.WriteString(string(label))
	dst.WriteByte('\n')
}

func writeOp1(dst *bytes.Buffer, instr ir.Instr) {
	dst.Write(instr.Name)
	dst.WriteByte(' ')
	dst.WriteString(strconv.FormatUint(uint64(instr.Data), 10))
	dst.WriteByte('\n')
}

func writeOp0(dst *bytes.Buffer, instr ir.Instr) {
	dst.Write(instr.Name)
	dst.WriteByte('\n')
}
