package dt

import (
	"bytes"
	"fmt"
)

// ExecutionStack emulates Elisp execution stack.
// Used to generate stack-ref instructions.
type ExecutionStack struct {
	names  []string
	maxLen int
}

func (st *ExecutionStack) Len() int {
	return len(st.names)
}

func (st *ExecutionStack) Clear() {
	st.names = st.names[:0]
	st.maxLen = 0
}

// MaxLen returns max stack length recorded.
func (st *ExecutionStack) MaxLen() int {
	return st.maxLen
}

// Bind assigns name for last pushed value.
func (st *ExecutionStack) Bind(name string) {
	st.names[len(st.names)-1] = name
}

// Rebind re-assigns name for specified stack element.
func (st *ExecutionStack) Rebind(ref int, name string) {
	maxIndex := len(st.names) - 1
	st.names[maxIndex-ref] = name
}

// Push adds new unnamed value to stack.
func (st *ExecutionStack) Push() {
	st.push("")
}

// PushVar is like calling Push followed by a Bind(name).
func (st *ExecutionStack) PushVar(name string) {
	st.push(name)
}

// Dup pushes specified stack element (copies it).
func (st *ExecutionStack) Dup(ref uint16) {
	maxIndex := len(st.names) - 1
	st.push(st.names[maxIndex-int(ref)])
}

// PushConst is like Push, but used for constant refs.
func (st *ExecutionStack) PushConst(index uint16) {
	st.push(fmt.Sprintf("<%d>", index))
}

// Discard drops last N stack elements.
func (st *ExecutionStack) Discard(n uint16) {
	st.names = st.names[:len(st.names)-int(n)]
}

// Replace = stack.rebind(index-1, stack.pop()).
func (st *ExecutionStack) Replace(index uint16) {
	name := st.names[len(st.names)-1]
	st.Discard(1)
	st.Rebind(int(index-1), name)
}

// Find lookups binding and returns its ref index (not normal index).
func (st *ExecutionStack) Find(name string) int {
	maxIndex := len(st.names) - 1
	for i := maxIndex; i >= 0; i-- {
		if st.names[i] == name {
			return maxIndex - i
		}
	}
	return -1
}

func (st *ExecutionStack) push(val string) {
	st.names = append(st.names, val)
	if len(st.names) > st.maxLen {
		st.maxLen = len(st.names)
	}
}

func (st *ExecutionStack) String() string {
	buf := bytes.Buffer{}
	buf.WriteByte('[')
	for _, name := range st.names {
		if name == "" {
			buf.WriteString("? ")
		} else {
			buf.WriteString(name)
			buf.WriteByte(' ')
		}
	}
	buf.WriteByte(']')
	return buf.String()
}
