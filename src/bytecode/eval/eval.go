package eval

import (
	"bytecode"
)

// Object evaluates bytecode object.
// Evaluation is necessary to replace each pseudo-op with
// real (or empty) instructions.
//
// It also can perform low-level optimizations.
func Object(o *bytecode.Object, argc int) {
	st := newStack()
	for i := 0; i < argc; i++ {
		st.PushArg(i)
	}
	ev := evaluator{
		Object: o,
		stack:  st,
		argc:   argc,
	}
	ev.eval()
}
