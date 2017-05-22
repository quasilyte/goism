package compiler

import (
	"fmt"
	"ir/instr"
)

type label string

func labelCreate(cl *Compiler, name string) label {
	cl.lastLabelID++
	return label(fmt.Sprintf("&%s-%d", name, cl.lastLabelID))
}

func labelBind(cl *Compiler, label label) {
	writeLabel(&cl.buf, instr.Label, label)
}
