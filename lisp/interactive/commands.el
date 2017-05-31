;; {{ define "public/commands" }}
;; Interactive commands.

(eval-when-compile
  (defmacro Go--cmd-exit-code (res)
    `(car ,res))
  (defmacro Go--cmd-output (res)
    `(cdr ,res)))

(defun Go-translate-by-path (pkg-path)
  "Read Go package located at PKG-PATH and translate it into Emacs Lisp.
Generated code is shown in temporary buffer.
Note that this method depends on GOPATH environment variables.
Requires `goel_translate_package' to be available."
  (interactive "DGo package path: ")
  (let* ((pkg-path (expand-file-name pkg-path))
         (res (Go--exec
               "goel_translate_package"
               (format "-pkgPath=%s" pkg-path))))
    (Go--ir-pkg-compile (read (Go--cmd-output res)))))

(defun Go-translate-by-name (pkg-name)
  "Like `Go-translate-by-path', but prepends `Go-emacs-package-path'
to specified package name."
  (interactive "sGo package name: ")
  (Go-translate-by-path (concat Go-emacs-package-path "/" pkg-name)))

(defun Go-disassemble-by-path (pkg-path &optional disable-opt)
  "Read Go package located at PKG-PATH and print its IR.
Output is shown in temporary buffer.
Requires `goel_translate_package' to be available."
  (interactive "DGo package path: ")
  (let* ((pkg-path (expand-file-name pkg-path))
         (opt-arg (if disable-opt "false" "true"))
         (res (Go--exec
               "goel_translate_package"
               "-output=asm"
               (format "-pkgPath=%s" pkg-path)
               (format "-opt=%s" opt-arg))))
    (with-output-to-temp-buffer "*IR compile*"
      (princ (Go--cmd-output res)))))

(defun Go-disassemble-by-name (pkg-name)
  "Like `Go-disassemble-by-path', but prepends `Go-emacs-package-path'
to specified package name."
  (interactive "sGo package name: ")
  (Go-disassemble-by-path (concat Go-emacs-package-path "/" pkg-name)))

(defun Go--exec (cmd &rest args)
  (let* ((cmd (if (string= "" Go-utils-path)
                  cmd
                (format "%s/%s" Go-utils-path cmd)))
         (res (with-temp-buffer
                (cons (apply #'call-process cmd nil t nil args)
                      (buffer-string)))))
    (when (/= 0 (Go--cmd-exit-code res))
      (error (Go--cmd-output res)))
    res))

;; {{ end }}
