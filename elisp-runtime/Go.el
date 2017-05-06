(define-error 'Go-panic "Go panic")

(defun Go-panic (error-data)
  (signal 'Go-panic (list error-data)))
