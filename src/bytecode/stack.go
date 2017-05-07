package bytecode

import "fmt"

// Compile-time execution stack.
// Emulates execution (data) stack used in Emacs VM.
type stack struct {
	vals    []string
	maxSize int
}

var constRefs []string

func init() {
	constRefs = make([]string, 99)
	for i := range constRefs {
		constRefs[i] = fmt.Sprintf("<%d>", i)
	}
}

func (st *stack) pushVar(name string) {
	st.vals = append(st.vals, name)

	if st.maxSize < len(st.vals) {
		st.maxSize = len(st.vals)
	}
}

func (st *stack) pushConst(index int) {
	if index < len(constRefs) {
		st.pushVar(constRefs[index])
	} else {
		st.pushVar("<99+>")
	}
}

func (st *stack) pushTmp() {
	st.pushVar(":tmp")
}

func (st *stack) drop(n int) {
	st.vals = st.vals[:len(st.vals)-n]
}

func (st *stack) ref(index int) {
	st.vals = append(st.vals, st.vals[len(st.vals)-index])
}

func (st *stack) findVar(name string) int {
	top := len(st.vals) - 1
	for i := top; i >= 0; i-- {
		if st.vals[i] == name {
			// Substract to get distance from the stack top.
			return top - i
		}
	}
	return -1
}
