`Go->Emacs Lisp` translated code is called `GE` inside 
this document for brevity.
`Emacs Lisp` here spelled as `Elisp` for the same reason 
even if it is innacurate generally.

## Constants

Go constants are inlined at the compilation time.
They do not produce `defvar` or `defconst`,
therefore can not be used inside Elisp.
