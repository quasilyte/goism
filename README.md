# Go.el
Not a fan of Emacs Lisp? Hack Emacs in Go!

## How it works

Valid Go package is converted
into Emacs Lisp sources that can be used inside Emacs.

Emacs `Go` (first letter is capital, thats important) package
implements Go runtime, so translated code behaves as
close to the [specs](https://golang.org/ref/spec) as possible.

To run Go inside Emacs, you need to evaluate 
`(Go-load-package PKG-NAME)` and all functions,
types and constants become available.

Symbols from Go are loaded as: `"Go-" PKG-NAME "." SYM-NAME`.
For example, `foo` package function `bar` 
is loaded as `Go-foo.bar`.

Polite `Go.el` code exports Emacs-friendly API that lisp
objects as an arguments. In the cases where Go types are 
expected, you may want to use type coercions.

## Go "emacs" package

Go code written for Emacs interacts with it using `emacs` package.
This package exports some neat functions, for example, 
you can call any Lisp function from you code.

Emacs package is pretty special, it has stub source files
with commentaries, but the implementation is external 
(compiler generates special code for them).

You can use existing Go code in your `Go.el` packages,
including most of the Go standard library.
Limitations will be posted later; support range is going
to be extended over time.
