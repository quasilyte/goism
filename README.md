![Logo](misc/logo.png)

[![License](http://img.shields.io/:license-MIT-blue.svg?style=flat)](LICENSE)

# goism 

Searching for Emacs Lisp alternative? **Try hacking Emacs in Go!**

## Overview

### Description

`goism` is Emacs package that makes it possible to use 
[Go](https://golang.org/) programming language instead
of Emacs Lisp inside Emacs. 

It provides Go intrinsics and `emacs` package to make it
possible to control Emacs from your programs.
Generated functions, methods and variables can be accessed from
Emacs Lisp code.

Enjoy the increased type safety and curly braces!

### How it works

Valid Go package is converted into Emacs Lisp bytecode.

Emacs `goism` package implements Go runtime, 
so translated code behaves as
close to the [specs](https://golang.org/ref/spec) as possible.

Different optimizations are performed during this translation,
so it is not going to be any slower than "native" Emacs Lisp.

### How to use it

[See quick guide](docs/quick_guide.md).

> TODO: emacs package installation

## Docs

* [Quick guide (getting started)](docs/quick_guide.md)
* [Translation specification](docs/translation_spec.md)
* [Features that are not implemented](docs/unimplemented.md)
* [goism implementation design](docs/design.md)

To see what features are going to be implemented in near future,
check out [Projects](https://github.com/Quasilyte/goism/projects) tab.

[Roadmap 2](https://github.com/Quasilyte/goism/projects/2) is
the current stage.

## Tags

* Compile Golang to Emacs Lisp bytecode
* Golang from Emacs
* Emacs Lisp alternative to extend Emacs
* Emacs Lisp as Golang compilation target
* "Go" emacs package
