package export

import (
	"bytes"
	"strconv"
)

const (
	scratchBufSize    = 64
	shortStringMaxLen = scratchBufSize - 3
)

type writer struct {
	buf bytes.Buffer
	tmp [scratchBufSize]byte
}

func (w *writer) Bytes() []byte {
	return w.buf.Bytes()
}

func (w *writer) WriteString(val string) {
	if len(val) <= shortStringMaxLen {
		// Faster path for short strings.
		w.tmp[0] = '"'
		copy(w.tmp[1:], val)
		w.tmp[len(val)+1] = '"'
		w.tmp[len(val)+2] = ' '
		w.buf.Write(w.tmp[:len(val)+3])
	} else {
		w.buf.WriteByte('"')
		w.buf.WriteString(val)
		w.buf.WriteString(`" `)
	}
}

func (w *writer) WriteByte(val byte) {
	w.buf.WriteByte(val)
}

func (w *writer) Write(val []byte) {
	w.buf.Write(val)
	w.buf.WriteByte(' ')
}

func (w *writer) WriteSymbol(val string) {
	w.buf.WriteString(val)
	w.buf.WriteByte(' ')
}

func (w *writer) WriteInt(val int) {
	w.buf.WriteString(strconv.FormatInt(int64(val), 10))
	w.buf.WriteByte(' ')
}
