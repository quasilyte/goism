`Go->Emacs Lisp` translated code is called `GE` inside 
this document for brevity.
`Emacs Lisp` here spelled as `Elisp` for the same reason 
even if it is innacurate generally.

## Constants

Elisp has no real constants. Any symbol can be changed
during runtime. 

Constants can be inlined only if they are immutable.
To make GE efficient, constants must be inlineable,
thus compiler mangles constant symbols in a special way.

To read exported Go constant value inside Elisp,
`Go-const` macro is used.
