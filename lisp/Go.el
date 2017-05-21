;;; -*- lexical-binding: t -*-

(eval-when-compile
  (defmacro not-eq (x y)
    `(not (eq ,x ,y))))

;;; ------------------
;;; [Public functions]

;;; ------------------------
;;; [Runtime implementation]

(defvar Go--ret-2 nil)
(defvar Go--ret-3 nil)
(defvar Go--ret-4 nil)
(defvar Go--ret-5 nil)
(defvar Go--ret-6 nil)
(defvar Go--ret-7 nil)
(defvar Go--ret-8 nil)

;; [Print]

(defun Go--print (&rest args)
  (princ (mapconcat #'prin1-to-string args ""))
  nil)

(defun Go--println (&rest args)
  (princ (mapconcat #'prin1-to-string args " "))
  (terpri)
  nil)

;; [Panic]

(define-error 'Go--panic "Go panic")

(defun Go--panic (error-data)
  (signal 'Go--panic (list error-data)))

(defun Go--!lisp-type-assert (x expected-typ)
  (Go--panic (format "interface conversion: lisp.Object is %s, not %s"
                    (Go--lisp-typename x)
                    expected-typ)))

(defun Go--!object-int (x)
  (Go--!lisp-type-assert x "lisp.Int"))
(defun Go--!object-float (x)
  (Go--!lisp-type-assert x "lisp.Float"))
(defun Go--!object-string (x)
  (Go--!lisp-type-assert x "lisp.String"))
(defun Go--!object-symbol (x)
  (Go--!lisp-type-assert x "lisp.Symbol"))

(defun Go--lisp-typename (x)
  (cond ((integerp x) "lisp.Int")
        ((floatp x) "lisp.Float")
        ((stringp x) "lisp.String")
        ((symbolp x) "lisp.Symbol")
        (t (error "`%s' is not instance of lisp.Object" (type-of x)))))

;; [Map]

(defun Go--make-map ()
  (make-hash-table :test #'equal))
(defun Go--make-map-cap (cap)
  (make-hash-table :size cap :test #'equal))

(defun Go--map-insert (key val m)
  (if (not-eq Go--nil-map m)
      (puthash key val m)
    (Go--panic "assignment to entry in nil map")))

;; [Nil values]

(defvar Go--nil-map (make-hash-table :size 1 :test #'eq))
