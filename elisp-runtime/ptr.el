;; Pointers emulation.

(defmacro Go-make-ptr (val) `(cons ,val nil))
(defmacro Go-ptr-deref (ptr) `(car ,ptr))
(defmacro Go-ptr-set (ptr val) `(setcar ,ptr ,val))
