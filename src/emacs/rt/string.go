package rt

import (
	"emacs/lisp"
)

// StringGet returns string nth byte.
// Result is not truncated to "byte" type, rune is returned.
func StringGet(s string, index int) rune {
	if !lisp.IsMultibyteString(s) {
		return lisp.ArefString(s, index) // Fast path
	}
	return MbStringGet(s, index)
}

// MbStringGet is a part of StringIndex implementation.
// Called for multibyte strings.
func MbStringGet(s string, index int) rune {
	length := lisp.StringBytes(s)
	if index <= length {
		return mbStringGetFwd(s, index)
	}
	return mbStringGetRev(s, index, length)
}

// Part of MbStringGet implementation.
func mbStringGetFwd(s string, index int) rune {
	i := 1                      // Current Emacs Lisp string index
	ch := lisp.ArefString(s, 0) // Last inspected character
	size := utf8CharWidth(ch)   // Size of "ch"
	offset := size              // Amount of bytes traversed
	for offset <= index {
		ch = lisp.ArefString(s, i)
		size = utf8CharWidth(ch)
		offset += size
		i++
	}
	return utf8DecodeByte(ch, size-(offset-index), size)
}

// Part of MbStringGet implementation.
// String traversal is done in reverse order.
func mbStringGetRev(s string, index, length int) rune {
	i := lisp.Length(s) - 2
	ch := lisp.ArefString(s, lisp.Length(s)-1)
	size := utf8CharWidth(ch)
	offset := length - size - 1
	for offset >= index {
		ch = lisp.ArefString(s, i)
		size = utf8CharWidth(ch)
		offset -= size
		i--
	}
	return utf8DecodeByte(ch, (index-offset)-1, size)
}
