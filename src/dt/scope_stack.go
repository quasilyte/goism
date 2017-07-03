package dt

type ScopeStack struct {
	depths []int
	depth  int
}

func (ss *ScopeStack) SetScopeDepth(depth int) {
	ss.depth = depth
}

func (ss *ScopeStack) PopScope() int {
	depth := ss.depth
	ss.drop()
	return depth
}

func (ss *ScopeStack) PushScope() {
	ss.depths = append(ss.depths, ss.depth)
	ss.depth = 0
}

func (ss *ScopeStack) drop() {
	ss.depth = ss.depths[len(ss.depths)-1]
	ss.depths = ss.depths[:len(ss.depths)-1]
}
