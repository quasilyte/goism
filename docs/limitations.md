## Limitations

Note that some of the limitations may be fixed in future.

### Float32

During typechecking, `float32` and `float64` types are incompatible.
After translation to Emacs Lisp, there is no more `float32`,
only double precision `float64`. 
