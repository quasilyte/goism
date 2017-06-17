package dt

import (
	"bytes"
	"fmt"
)

// DataStack emulates Elisp data (parameter) stack.
// Used to generate stack-ref instructions.
type DataStack struct {
	names  []string
	maxLen int
}

// NewDataStack creates new stack that is
// populated by provided bindings.
func NewDataStack(bindings []string) *DataStack {
	names := make([]string, 0, len(bindings)+8)
	return &DataStack{
		names:  append(names, bindings...),
		maxLen: len(bindings),
	}
}

// Len returns current stack size.
func (st *DataStack) Len() int {
	return len(st.names)
}

// MaxLen returns max stack length recorded.
func (st *DataStack) MaxLen() int {
	return st.maxLen
}

// Bind assigns name for last pushed value.
func (st *DataStack) Bind(name string) {
	st.names[len(st.names)-1] = name
}

// Rebind re-assigns name for specified stack element.
func (st *DataStack) Rebind(ref int, name string) {
	maxIndex := len(st.names) - 1
	st.names[maxIndex-ref] = name
}

// Push adds new unnamed value to stack.
func (st *DataStack) Push() {
	st.push("")
}

// Dup pushes specified stack element (copies it).
func (st *DataStack) Dup(ref uint16) {
	maxIndex := len(st.names) - 1
	st.push(st.names[maxIndex-int(ref)])
}

// PushConst is like Push, but used for constant refs.
func (st *DataStack) PushConst(index uint16) {
	st.push(fmt.Sprintf("<%d>", index))
}

// Discard drops last N stack elements.
func (st *DataStack) Discard(n uint16) {
	st.names = st.names[:len(st.names)-int(n)]
}

// Replace = stack.rebind(index-1, stack.pop()).
func (st *DataStack) Replace(index uint16) {
	name := st.names[len(st.names)-1]
	st.Discard(1)
	st.Rebind(int(index-1), name)
}

// Find lookups binding and returns its ref index (not normal index).
func (st *DataStack) Find(name string) int {
	maxIndex := len(st.names) - 1
	for i := maxIndex; i >= 0; i-- {
		if st.names[i] == name {
			return maxIndex - i
		}
	}
	return -1
}

// String returns human-readable stack representation.
// Useful for debug.
func (st *DataStack) String() string {
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

func (st *DataStack) push(val string) {
	st.names = append(st.names, val)
	if len(st.names) > st.maxLen {
		st.maxLen = len(st.names)
	}
}
