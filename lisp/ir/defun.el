;; {{ define "ir/defun" }}

(defmacro Go--defun-ir (name args depth cvec &rest instrs)
  (declare (indent defun))
  (let* ((instrs (nconc instrs '(end)))
         (args-desc (byte-compile-make-args-desc args))
         (bytecode (byte-compile-lapcode
                    (byte-optimize-lapcode
                     (Go--ir-to-lapcode instrs)))))
    `(defalias ',name
       ,(make-byte-code args-desc
                        bytecode
                        cvec
                        depth))))

;; {{ end }}
