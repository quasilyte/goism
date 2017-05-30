;; {{ define "rt/type-assert" }}
;; Implementation of type assertions.

(defun Go--!lisp-type-assert (x expected-typ)
  (Go--panic (format "interface conversion: lisp.Object is %s, not %s"
                    (Go--lisp-typename x)
                    expected-typ)))

(defun Go--!object-int (x)
  (Go--!lisp-type-assert x "lisp.Int"))
(defun Go--!object-float (x)
  (Go--!lisp-type-assert x "lisp.Float"))
(defun Go--!object-string (x)
  (Go--!lisp-type-assert x "lisp.Str"))
(defun Go--!object-symbol (x)
  (Go--!lisp-type-assert x "lisp.Symbol"))

(defun Go--lisp-typename (x)
  (cond ((integerp x) "lisp.Int")
        ((floatp x) "lisp.Float")
        ((stringp x) "lisp.Str")
        ((symbolp x) "lisp.Symbol")
        (t (error "`%s' is not instance of lisp.Object" (type-of x)))))

;; {{ end }}
