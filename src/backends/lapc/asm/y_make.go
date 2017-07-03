package asm

import (
	"assert"
	"backends/lapc/ir"
	"dt"
)

type yUnit []ir.Instr

type converterY struct {
	instrs []ir.Instr
	st     *dt.DataStack

	inlineRets []branchInfo
	gotos      []branchInfo
	labels     []labelInfo
	scopes     *dt.ScopeStack
	iifeDepths *dt.ScopeStack
}

type branchInfo struct {
	pos     int
	depth   int
	labelID int32
}

type labelInfo struct {
	depth int
}

func makeY(params []string, instrs xUnit) (yUnit, int) {
	conv := converterY{
		instrs:     instrs,
		st:         dt.NewDataStack(params),
		scopes:     &dt.ScopeStack{},
		iifeDepths: &dt.ScopeStack{},
	}
	return conv.Convert()
}

func (cy *converterY) Convert() (yUnit, int) {
	cy.convert()
	cy.fixBranches()
	return cy.instrs, cy.st.MaxLen()
}

func (cy *converterY) convert() {
	for i, ins := range cy.instrs {
		switch ins.Kind {
		case ir.ScopeEnter:
			cy.scopes.PushScope()
			cy.scopes.SetScopeDepth(cy.st.Len())
			cy.instrs[i] = ir.Instr{Kind: ir.Empty}

		case ir.ScopeLeave:
			depth := cy.scopes.PopScope()
			scopeSize := cy.st.Len() - depth
			cy.instrs[i] = ir.Instr{Kind: ir.Discard, Data: int32(scopeSize)}
			cy.st.Discard(uint16(scopeSize))

		case ir.XinlineEnter:
			cy.iifeDepths.PushScope()
			cy.iifeDepths.SetScopeDepth(cy.st.Len())
			cy.instrs[i] = ir.Instr{Kind: ir.Empty}

		case ir.XinlineRetLabel:
			depth := cy.iifeDepths.PopScope()
			cy.labels = append(cy.labels, labelInfo{
				depth: depth,
			})
			cy.instrs[i].Kind = ir.Label
			cy.st.Discard(uint16(cy.st.Len() - depth - 1))

		case ir.XinlineRet:
			info := branchInfo{pos: i, depth: cy.st.Len(), labelID: ins.Data}
			cy.inlineRets = append(cy.inlineRets, info)
			cy.instrs[i].Kind = ir.Jmp

		case ir.Xgoto:
			info := branchInfo{pos: i, depth: cy.st.Len(), labelID: ins.Data}
			cy.gotos = append(cy.gotos, info)
			cy.instrs[i].Kind = ir.Jmp

		case ir.XlocalRef:
			stIndex := cy.st.Lookup(ins.Meta)
			cy.instrs[i] = ir.Instr{Kind: ir.StackRef, Data: int32(stIndex)}
			cy.st.Push()
			cy.st.Bind(ins.Meta)

		case ir.XlocalSet:
			stIndex := cy.st.Lookup(ins.Meta)
			cy.instrs[i] = ir.Instr{Kind: ir.StackSet, Data: int32(stIndex)}
			cy.st.Discard(1)

		case ir.XvarRef:
			cy.st.Push()
			cy.st.Bind(ins.Meta)

		case ir.XvarSet:
			cy.st.Discard(1)

		case ir.Xbind:
			cy.st.Bind(ins.Meta)
			cy.instrs[i] = ir.Instr{Kind: ir.Empty}

		case ir.Label:
			cy.labels = append(cy.labels, labelInfo{
				depth: cy.st.Len(),
			})
		}

		cy.simulateInstr(ins)
	}
}

func (cy *converterY) fixBranches() {
	for _, br := range cy.gotos {
		label := cy.labels[br.labelID]
		assert.True(br.depth >= label.depth)
		if br.depth == label.depth {
			continue
		}
		cy.instrs[br.pos-1] = ir.Instr{
			Kind: ir.Discard,
			Data: int32(br.depth - label.depth),
		}
	}

	for _, br := range cy.inlineRets {
		label := cy.labels[br.labelID]
		diff := int32(br.depth - label.depth)
		if diff == 1 {
			continue
		}
		cy.instrs[br.pos-2] = ir.Instr{Kind: ir.StackSet, Data: diff - 1}
		if diff > 2 {
			cy.instrs[br.pos-1] = ir.Instr{Kind: ir.Discard, Data: diff - 2}
		}
	}
}

func (cy *converterY) simulateInstr(ins ir.Instr) {
	st := cy.st
	enc := ir.EncodingOf(ins.Kind)

	switch enc.Input {
	case ir.AttrTakeNothing:
		// Do nothing
	case ir.AttrTake1:
		st.Discard(1)
	case ir.AttrTake2:
		st.Discard(2)
	case ir.AttrTake3:
		st.Discard(3)
	case ir.AttrTakeN:
		st.Discard(uint16(ins.Data))
	case ir.AttrTakeNplus1:
		st.Discard(uint16(ins.Data + 1))
	case ir.AttrReplaceNth:
		st.Replace(uint16(ins.Data))
	}

	switch enc.Output {
	case ir.AttrPushNothing:
		// Do nothing
	case ir.AttrDupNth:
		st.Dup(uint16(ins.Data))
	case ir.AttrPushTmp:
		st.Push()
	case ir.AttrPushConst:
		st.PushConst(uint16(ins.Data))
	case ir.AttrPushAndDiscard:
		st.Push()
		cy.simulateInstr(ir.Instr{Kind: ir.Discard, Data: 1})
	}
}
