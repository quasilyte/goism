package symbols

const symPrefix = "goism-"

// Mangle returns Emacs Lisp symbol name that is suitable for intern.
func Mangle(pkgPath string, name string) string {
	totalLen := len(symPrefix) + len(pkgPath) + len(".") + len(name)
	buf := make([]byte, totalLen)

	pos := 0
	copy(buf[pos:], symPrefix)
	pos += len(symPrefix)
	copy(buf[pos:], pkgPath)
	pos += len(pkgPath)
	copy(buf[pos:], ".")
	pos += len(".")
	copy(buf[pos:], name)

	return string(buf)
}
