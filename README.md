![Logo](misc/logo.png)

# Go.el
Not a fan of Emacs Lisp? Hack Emacs in Go!

## What?

`Go.el` is Emacs package that makes it possible to use 
[Go](https://golang.org/) programming language instead
of Emacs Lisp inside Emacs. 

## How?

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

## Docs

* [Limitations](docs/limitations.md)
* [Translation specification](docs/translation_spec.md)
* [Go "emacs" package overview](docs/go_emacs.md)
* [Emacs "Go" library](docs/emacs_go.md)
