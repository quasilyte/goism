# Design overview

## Overall 

The whole process can be described as a sequence of steps
that transform input into output:

1) Loading input: Go package
2) Parsing and typechecking (handled by `go/parser` and other `go/*` packages)
3) Translating `go/ast` into `sexp` (S-expression) format
4) High-level optimizations on `sexp` 
5) Compilation of `sexp` into `bytecode/ir` with pseudo-ops
6) Evaluation of `bytecode/ir` (low-level optimizations + replacement of pseudo-ops)
7) Convertion of `bytecode/ir` into Emacs Lisp bytecode
8) Generating output: Emacs Lisp source file (done by `export` package)

It is possible to mitigate all intermediate steps and emit bytecode
right after (2) step, but it will make much harder to implement all
planned optimizations. Total amount of code will be less, but individual
package complexity will be higher. 

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

### Bytecode IR

IR is another step before generation of bytecode takes part.

The advantages of having an IR:
* Makes low-level optimizations possible
* Simplifies bytecode generation step
* Helps to achieve better separation of responsibilities

The pseudo operations is a consequence of difficulties to generate 
100% complete code during the single pass. 
We want second pass to apply optimizations anyway, 
so it is trivial to convert those instructions during that
stage. It also frees IR compiler from execution stack maintenance.
IR optimizer needs stack in any case; introducing pseudo ops
removes stack manipulations burden from the compiler.
