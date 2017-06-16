;;; -*- lexical-binding: t -*-

(require 'subr-x)

(defmacro goism-declare (&rest decls)
  "Generate `emacs/lisp/ffi.go' file contents."
  `(goism--declare (quote ,decls)))

;; A set of types that are permitted inside FFI signatures.
(defconst goism--decl-types
  '((:any . "any")         ;; Note: can not be used for output type
    (:object . "Object")   ;; lisp.Object interface type
    (:void . "")           ;; Discards function result right after the call
    (:symbol . "Symbol")
    (:bool . "bool")
    (:int . "int")
    (:string . "string")
    (:float . "float64")))

;; Extract commentary for Lisp symbol.
;; Format it as Go comment.
(defun goism--decl-func-comment (lisp-sym go-name)
  (let ((doc-string (or (documentation lisp-sym)
                        "extern Emacs Lisp function.")))
    (format "// %s = %s"
            go-name
            (thread-last doc-string
              (replace-regexp-in-string "\n\n(fn[^)]*)" "")
              (replace-regexp-in-string "\n" "\n// ")))))

;; Lookup Go type name for given type keyword.
(defun goism--decl-type-name (type)
  (if (vectorp type)
      (concat "func"
              (goism--decl-func-signature (aref type 0)
                                          (aref type 1)))
    (or (alist-get type goism--decl-types)
        (error "Unsupported type `%s'" type))))

;; Validates and then returns back passed identifier name.
(defun goism--decl-ident (name)
  (unless (string-match-p "^[_[:alpha:]][_0-9[:alpha:]]*$" name)
    (error "invalid Go identifier `%s'" name))
  name)

;; Returns a string that contains single function parameter declaration.
(defun goism--decl-param (type name)
  (concat (if (string-prefix-p "&" name)
              (concat (goism--decl-ident (substring name 1))
                      " ...")
            (concat (goism--decl-ident name)
                    " "))
          (goism--decl-type-name type)))

;; Returns function signature as string.
(defun goism--decl-func-signature (input-spec output-spec)
  (when (eq :any output-spec)
    (error "Can not use `:any' type as function output type"))
  (setq input-spec (seq-partition input-spec 2))
  (format "(%s) %s"
          (mapconcat (lambda (x)
                       (let ((type (nth 0 x))
                             (param-name (symbol-name (nth 1 x))))
                         (goism--decl-param type param-name)))
                     input-spec
                     ", ")
          (goism--decl-type-name output-spec)))

;; Implementation of `goism-declare' macro.
(defun goism--declare (decls)
  (with-output-to-temp-buffer "*ffi.go*"
    (princ (concat "package lisp\n"
                   "\n"
                   "// This package is special.\n"
                   "// It may not be a good idea to edit it by manually.\n"
                   "\n"))
    (dolist (decl decls)
      (let* ((lisp-sym (pop decl))
             (go-name (goism--decl-ident (symbol-name (pop decl))))
             (input-spec (pop decl))
             (output-spec (pop decl))
             (comment (goism--decl-func-comment lisp-sym go-name))
             (sig (goism--decl-func-signature input-spec output-spec)))
        (princ
         (concat comment
                 (format "\n//\n//goism:\"%s\"->\"%s\"\n" go-name lisp-sym)
                 "func "
                 go-name
                 sig
                 "\n\n"))))))
