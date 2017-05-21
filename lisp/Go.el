;;; -*- lexical-binding: t -*-

(eval-when-compile
  (defmacro not-eq (x y)
    `(not (eq ,x ,y)))
  (defmacro Go--cmd-exit-code (res)
    `(car ,res))
  (defmacro Go--cmd-output (res)
    `(cdr ,res)))

;;; ------------------
;;; [Public functions]

(defcustom Go-utils-path ""
  "Specifies path which is used to find required binaries.
Please note that this path *must* end with trailing slash.
If remain empty, system PATH is used.")

(defun Go-translate-package (pkg-path)
  "Read Go package located at PKG-PATH and translate it into Emacs Lisp.
Generated code is shown in temporary buffer.
Requires `goel-translate-package' to be available."
  (interactive "DGo package path: ")
  (let* ((pkg-path (expand-file-name pkg-path))
         (res (Go--exec
               "goel-translate-package"
               (format "-pkgPath=%s" pkg-path))))
    (ir--pkg-compile (read (Go--cmd-output res)))))

(defun Go-disassemble-package (pkg-path &optional disable-opt)
  "Read Go package located at PKG-PATH and print its IR.
Output is shown in temporary buffer.
Requires `goel-translate-package' to be available."
  (interactive "DGo package path: ")
  (let* ((pkg-path (expand-file-name pkg-path))
         (opt-arg (if disable-opt "false" "true"))
         (res (Go--exec
               "goel-translate-package"
               "-output=asm"
               (format "-pkgPath=%s" pkg-path)
               (format "-opt=%s" opt-arg))))
    (with-output-to-temp-buffer "*IR compile*"
      (princ (Go--cmd-output res)))))

;;; -------------------
;;; [Private functions]

(defun Go--exec (cmd &rest args)
  (let* ((cmd (format "%s%s" Go-utils-path cmd))
         (res (with-temp-buffer
                (cons (apply #'call-process cmd nil t nil args)
                      (buffer-string)))))
    (when (/= 0 (Go--cmd-exit-code res))
      (error (Go--cmd-output res)))
    res))

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
