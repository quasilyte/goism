package eval

const nilID = 0xFFFF

// Used to represent item value which can not be examined
// at compile time.
type unknownValue struct{}

var unknownItem = item{
	val:     unknownValue{},
	localID: nilID,
}

// Item is an arbitrary execution stack value.
type item struct {
	val     interface{}
	localID uint16
}

type stack struct {
	items  []item
	maxLen int
}

func newStack() stack {
	return stack{
		items: make([]item, 0, 32),
	}
}

func (st *stack) Len() int {
	return len(st.items)
}

func (st *stack) BindLocal(id uint16) {
	index := st.Len() - 1
	st.items[index].localID = id
}

func (st *stack) Copy(index int) {
	st.items = append(st.items, st.items[index])
}

func (st *stack) FindLocal(id uint16) int {
	for i := st.Len() - 1; i >= 0; i-- {
		if st.items[i].localID == id {
			return i
		}
	}
	return -1
}

func (st *stack) Push(x interface{}) {
	st.push(item{
		val:     x,
		localID: nilID,
	})
}

func (st *stack) PushUnknown() {
	st.push(unknownItem)
}

func (st *stack) PushArg(id int) {
	st.push(item{
		val:     unknownValue{},
		localID: uint16(id),
	})
}

func (st *stack) Drop(n int) {
	st.items = st.items[:len(st.items)-n]
}

func (st *stack) Pop() item {
	item := st.items[st.Len()-1]
	st.items = st.items[:st.Len()-1]
	return item
}

func (st *stack) push(x item) {
	st.items = append(st.items, x)
	if len(st.items) > st.maxLen {
		st.maxLen = len(st.items)
	}
}
