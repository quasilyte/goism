package asm

import (
	"assert"
	"backends/lapc/ir"
	"dt"
)

type yUnit *ir.Instr

type converterY struct {
	st *dt.DataStack

	inlineRets []branchInfo
	gotos      []branchInfo
	labels     map[int32]labelInfo
	scopes     *dt.ScopeStack
	iifeDepths *dt.ScopeStack
}

type branchInfo struct {
	ins   *ir.Instr
	depth int
}

type labelInfo struct {
	depth int
}

func makeY(params []string, u xUnit) (yUnit, int) {
	conv := converterY{
		st:         dt.NewDataStack(params),
		scopes:     &dt.ScopeStack{},
		iifeDepths: &dt.ScopeStack{},
		labels:     make(map[int32]labelInfo),
	}
	return conv.Convert(u)
}

func (cy *converterY) Convert(u xUnit) (yUnit, int) {
	cy.convert(u)
	cy.fixBranches()
	return yUnit(u), cy.st.MaxLen()
}

func (cy *converterY) convert(u xUnit) {
	for ins := u; ins != nil; ins = ins.Next {
		cy.convertInstr(ins)
		cy.simulateInstr(ins)
	}
}

func (cy *converterY) convertInstr(ins *ir.Instr) {
	switch ins.Kind {
	case ir.ScopeEnter:
		cy.scopes.PushScope()
		cy.scopes.SetScopeDepth(cy.st.Len())
		ins.Remove()

	case ir.ScopeLeave:
		depth := cy.scopes.PopScope()
		scopeSize := cy.st.Len() - depth
		ins.Kind, ins.Data = ir.Discard, int32(scopeSize)

	case ir.XinlineEnter:
		cy.iifeDepths.PushScope()
		cy.iifeDepths.SetScopeDepth(cy.st.Len())
		ins.Remove()

	case ir.XinlineRetLabel:
		depth := cy.iifeDepths.PopScope()
		cy.labels[ins.Data] = labelInfo{depth: depth}
		ins.Kind = ir.Label
		cy.st.Discard(uint16(cy.st.Len() - depth - 1))

	case ir.XinlineRet:
		info := branchInfo{ins: ins, depth: cy.st.Len()}
		cy.inlineRets = append(cy.inlineRets, info)
		ins.Kind = ir.Jmp

	case ir.Xgoto:
		info := branchInfo{ins: ins, depth: cy.st.Len()}
		cy.gotos = append(cy.gotos, info)
		ins.Kind = ir.Jmp

	case ir.XlocalRef:
		stIndex := cy.st.Lookup(ins.Meta)
		ins.Kind, ins.Data = ir.StackRef, int32(stIndex)

	case ir.XlocalSet:
		stIndex := cy.st.Lookup(ins.Meta)
		ins.Kind, ins.Data = ir.StackSet, int32(stIndex)

	case ir.XvarRef:
		cy.st.Push()
		cy.st.Bind(ins.Meta)

	case ir.XvarSet:
		cy.st.Discard(1)

	case ir.Xbind:
		cy.st.Bind(ins.Meta)
		ins.Remove()

	case ir.Label:
		cy.labels[ins.Data] = labelInfo{depth: cy.st.Len()}
	}
}

func (cy *converterY) fixBranches() {
	for _, br := range cy.gotos {
		label := cy.labels[br.ins.Data]
		assert.True(br.depth >= label.depth)
		if br.depth == label.depth {
			continue
		}
		br.ins.InsertPrev(&ir.Instr{
			Kind: ir.Discard,
			Data: int32(br.depth - label.depth),
		})
	}

	for _, br := range cy.inlineRets {
		label := cy.labels[br.ins.Data]
		diff := int32(br.depth - label.depth)
		assert.True(diff > 0)
		switch diff {
		case 1:
			// Do nothing
		case 2:
			br.ins.InsertPrev(&ir.Instr{Kind: ir.StackSet, Data: diff - 1})
		default:
			br.ins.InsertLeft([]*ir.Instr{
				&ir.Instr{Kind: ir.StackSet, Data: diff - 1},
				&ir.Instr{Kind: ir.Discard, Data: diff - 2},
			})
		}
	}
}

func (cy *converterY) simulateInstr(ins *ir.Instr) {
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
		ins.InsertNext(&ir.Instr{Kind: ir.Discard, Data: 1})
	}
}
