Go code written for Emacs interacts with it using `emacs` package.
This package exports some neat functions, for example, 
you can call any Lisp function from you code.

Emacs package is pretty special, it has stub source files
with commentaries, but the implementation is external 
(compiler generates special code for them).

You can use existing Go code in your `goism` packages,
including most of the Go standard library.
Limitations will be posted later; support range is going
to be extended over time.

For now, source code is a best API reference you can get.
