# Translation spec

This document describes design decisions that affect
either **Go spec confirmity** or **Emacs Lisp experience**.

## Abbreviations and conventions

`Go->Emacs Lisp` translated code is called `GE` inside 
this document for brevity.

`Emacs Lisp` here spelled as `Elisp` for the same reason 
even if it is innacurate generally.

* The most significant information presented in this way (UL)

## Implementation details

### (1) Constants

Go constants are inlined at the compilation time.
They do not produce `defvar` or `defconst`.

* You can not use Go constants inside Elisp

### (2) Numeric types

On Go level (compilation time), everything is the same,
but during the runtime, all types are implemented in terms
of Elisp numbers (int and float).

All signed types are implemented without special treatment.
This means that `int8`, ..., `int64` are simple Elisp integer.

* You can not rely on `intX` types overflow

Unsigned types are emulated. 
There is no overhead on arithmetics, the most significant
bits are cleared only when not doing 
so will affect *visible results*.

* `uint64` type behaves like `uint32`
* `float32` type behaves like `float64`

`float64` depends on the Elisp float,
which implemented in terms of C `double`. 
