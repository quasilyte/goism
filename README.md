![Logo](misc/logo.png)

# Go.el
Not a fan of Emacs Lisp? Hack Emacs in Go!

## Overview

#### What?

`Go.el` is Emacs package that makes it possible to use 
[Go](https://golang.org/) programming language instead
of Emacs Lisp inside Emacs. 

#### How?

Valid Go package is converted
into Emacs Lisp bytecode that can be used inside Emacs.

Emacs `Go` (first letter is capital, thats important) package
implements Go runtime, so translated code behaves as
close to the [specs](https://golang.org/ref/spec) as possible.

#### Go from Elisp

To run Go inside Emacs, you need to evaluate 
`(Go-load-package PKG-NAME)` and all package 
functions, types and variables become available.

Symbols from Go are loaded as: `"Go-" PKG-NAME "." SYM-NAME`.
For example, `foo` package function `Bar` 
is loaded as `Go-foo.Bar`.

#### Elisp from Go

When writing package specifically for Emacs, you
can use `emacs` package which exports some
fundamental primitives that let you do almost anything.
For example, `emacs.Call` invokes any Emacs function
with arbitrary argument list and returns Elisp object
(semantically, its `funcall`).

You can create and use Elisp objects:
`list := emacs.Call("cons", 1, 2)`.

This snippet inserts `"hello world"` into current buffer:
`emacs.Call("insert", "hello, world!")`.

## Docs

* [Limitations](docs/limitations.md)
* [Translation specification](docs/translation_spec.md)
* [Go "emacs" package overview](docs/go_emacs.md)
* [Emacs "Go" library](docs/emacs_go.md)
