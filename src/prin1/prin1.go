package prin1

import (
	"io"
	"sexp"
)

// Fprint writes given S expression to the writer.
// Written data is suitable for Lisp reader.
func Fprint(w io.Writer, object sexp.Node) (int64, error) {
	sw := sexpWriter{dst: w}
	sw.writeSexp(object)
	return sw.written, sw.err
}
