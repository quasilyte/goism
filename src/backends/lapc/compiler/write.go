package compiler

import (
	"backends/lapc/instr"
	"bytes"
	"strconv"
)

func writeLabel(dst *bytes.Buffer, op []byte, label label) {
	dst.Write(op)
	dst.WriteByte(' ')
	dst.WriteString(string(label))
	dst.WriteByte('\n')
}

func writeOp1(dst *bytes.Buffer, ins instr.Instr) {
	dst.Write(ins.Name)
	dst.WriteByte(' ')
	dst.WriteString(strconv.FormatUint(uint64(ins.Data), 10))
	dst.WriteByte('\n')
}

func writeOp0(dst *bytes.Buffer, ins instr.Instr) {
	dst.Write(ins.Name)
	dst.WriteByte('\n')
}
