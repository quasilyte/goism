# Design overview

## Overall 

The whole process can be described as a sequence of steps
that transform input into output:

1) Loading input: Go package
2) Parsing and typechecking (handled by `go/parser` and other `go/*` packages)
3) Translating `go/ast` into `sexp` (S-expression) format
4) High-level optimizations on `sexp` 
5) Compiling `sexp` with `ir/compiler` (IR format)
6) Generating output: IR package is written by `export` package

Next, `lisp/ir.el` can compile IR into Emacs Lisp source file.
This file can be either loaded or saved (for future use).

This project does not pursue compilation speed that much;
quality of code and simplicity of maintenance has far higher priority.

The rest of this document reveals some details on concrete steps and components.

## Detailed overview

### Sexp format

Sexp is a replacement for `go/ast` representation.

Sexp is closer to Lisps because instead of binary expressions for `+`
it has variadic ops. 

Other differences of `sexp` vs `go/ast`:
* Has flat structure. No need for nested switches
* Free of irrelevant data (like token positions)
* Most forms are types. For example, `sexp.Concat` implies string arguments
* Some forms are convenient specializations (e.g. `sexp.LispTypeAssert`)

The general idea is to get input in a shape that is convenient to work with.

### IR

IR is a format that resembles Elisp `lapcode` with prettier syntax
and additional information attached.

The advantages of having an IR:
* `lapcode` is more like private Emacs format; abstraction layer is needed
* Proposed format is easier to read
* May do additional tasks that simplify stages that are implemented in Go
