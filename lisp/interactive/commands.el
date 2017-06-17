;; {{ define "public/commands" }}
;; Interactive commands.

(eval-when-compile
  (defmacro goism--cmd-exit-code (res)
    `(car ,res))
  (defmacro goism--cmd-output (res)
    `(cdr ,res)))

(defun goism-translate (pkg-path)
  "Read Go package PKG-PATH and translate it into Emacs Lisp package.
Generated code is shown in temporary buffer.
Note that this method depends on GOPATH environment variables.
Requires `goism_translate_package' to be available.

Example: `(goism-translate \"example\")'"
  (interactive "sGo package: ")
  (let* ((pkg-path (concat "emacs/" pkg-path))
         (res (goism--exec
               "goism_translate_package"
               (format "-pkgPath=%s" pkg-path))))
    (goism--ir-pkg-compile (read (goism--cmd-output res)))))

(defun goism-load (pkg-path)
  "Calls `goism-translate', evaluates output buffer and then closes it.
Not recommended for untrusted packages."
  (interactive "sGo package: ")
  (goism-translate pkg-path)
  (with-current-buffer goism-output-buffer-name
    (eval-buffer)
    (kill-buffer-and-window)))

(defun goism-disassemble (pkg-path &optional disable-opt)
  "Read Go package PKG-PATH and print its IR.
Output is shown in temporary buffer.
Requires `goism_translate_package' to be available."
  (interactive "DGo package path: ")
  (let* ((pkg-path (concat "emacs/" pkg-path))
         (opt-arg (if disable-opt "false" "true"))
         (res (goism--exec
               "goism_translate_package"
               "-output=asm"
               (format "-pkgPath=%s" pkg-path)
               (format "-opt=%s" opt-arg))))
    (with-output-to-temp-buffer goism-output-buffer-name
      (princ (goism--cmd-output res)))))

(defun goism--exec (cmd &rest args)
  (let* ((cmd (if (string= "" goism-utils-path)
                  cmd
                (format "%s/%s" goism-utils-path cmd)))
         (env (cons (format "GOPATH=%s" (expand-file-name goism-emacs-gopath))
                    process-environment))
         (res (with-temp-buffer
                (let ((process-environment env))
                  (cons (apply #'call-process cmd nil t nil args)
                        (buffer-string))))))
    (when (/= 0 (goism--cmd-exit-code res))
      (error (goism--cmd-output res)))
    res))

;; {{ end }}
