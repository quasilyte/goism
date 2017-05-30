;; {{ define "rt/builtin" }}
;; Support for Go builtin functions.

(defun Go--print (&rest args)
  (princ (mapconcat #'prin1-to-string args ""))
  nil)

(defun Go--println (&rest args)
  (princ (mapconcat #'prin1-to-string args " "))
  (terpri)
  nil)

(define-error 'Go--panic "Go panic")

(defun Go--panic (error-data)
  (signal 'Go--panic (list error-data)))

;; {{ end }}
