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
can use `emacs` and `emacs/lisp` packages.

**Lisp** package is very low-level. 
It defines Emacs object types and **intrinsic** functions.

**Emacs** package is a big, typesafe convenience
wrapper around `emacs/lisp` package. Most of the time
you should use `emacs` package functions. 

```go
// You can create and use Elisp objects:
list := lisp.Call("cons", lisp.Int(1), lisp.String("2"))

// Insert "hello, world!" into current buffer:
lisp.Call("insert", lisp.String("hello, world!"))
// Luckily, "emacs" package has a nice wrapper:
emacs.Insert("hello, world")
```

## Docs

* [Translation specification](docs/translation_spec.md)
* [Features that are not implemented](docs/unimplemented.md)
* [Go "emacs" package overview](docs/go_emacs.md)
* [Emacs "Go" library](docs/emacs_go.md)
