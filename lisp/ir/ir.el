;; {{ define "ir/ir" }}
;; goism IR compiler.

;; Output IR package PKG to temp buffer.
;; Caller can decide to inspect/eval/save generated contents.
;; PKG is consumed.
(defun goism--ir-pkg-compile (pkg)
  (with-output-to-temp-buffer goism-output-buffer-name
    (let ((pkg-name (pop! pkg))
          (pkg-comment (pop! pkg)))
      (goism--ir-pkg-write-header pkg-name)
      (when (not (string= "" pkg-comment))
        (goism--ir-pkg-write-comment pkg-comment))
      (goism--ir-pkg-write-body pkg)
      (goism--ir-pkg-write-footer pkg-name))
    (with-current-buffer standard-output
      (emacs-lisp-mode)
      (setq buffer-read-only t))))

(defun goism--ir-pkg-write-header (pkg-name)
  (princ ";;; -*- lexical-binding: t -*-\n")
  (princ (format ";;; %s --- translated Go package\n" pkg-name))
  (princ ";; THIS CODE IS GENERATED, AVOID MANUAL EDITING!\n"))

(defun goism--ir-pkg-write-comment (pkg-comment)
  (princ "\n;;; Commentary:\n")
  (princ pkg-comment)
  (princ "\n\n;;; Code:\n"))

(defun goism--ir-pkg-write-footer (pkg-name)
  (princ (format "\n;;; %s ends here" pkg-name)))

(defun goism--ir-pkg-write-body (pkg)
  (let (token)
    (while (setq token (pop! pkg))
      (pcase token
        (`fn (goism--ir-pkg-write-fn pkg))
        (`vars (goism--ir-pkg-write-vars pkg))
        (`expr (goism--ir-pkg-write-expr pkg))
        (_ (error "Unexpected token `%s'" token))))))

(defun goism--ir-pkg-write-fn (pkg)
  (let* ((name (pop! pkg))
         (body (goism--ir-fn-body pkg)))
    (prin1 `(defalias ',name ,body))
    (terpri)))

(defun goism--ir-pkg-write-vars (pkg)
  (let (name)
    (while (not-eq 'end (setq name (pop! pkg)))
      (prin1 `(defvar ,name nil ""))
      (terpri))))

(defun goism--ir-pkg-write-expr (pkg)
  (let* ((cvec (pop! pkg))
         (stack-cap (pop! pkg))
         (bytecode (goism--ir-to-bytecode pkg cvec)))
    (prin1 `(byte-code ,bytecode ,cvec ,stack-cap))
    (terpri)))

(defun goism--ir-to-bytecode (pkg cvec)
  (byte-compile-lapcode
   (byte-optimize-lapcode
    (goism--ir-to-lapcode pkg cvec))))

(defun goism--ir-fn-body (pkg)
  (let* ((args-desc (pop! pkg))
         (cvec (pop! pkg))
         (stack-cap (pop! pkg))
         (doc-string (pop! pkg)))
    (make-byte-code args-desc
                    (goism--ir-to-bytecode pkg cvec)
                    cvec
                    stack-cap
                    doc-string)))

(defsubst goism--ir-make-info (kind data) (cons kind data))
(defsubst goism--ir-info-kind (info) (car info))
(defsubst goism--ir-info-data (info) (cdr info))

(defsubst goism--ir-make-env (cvec)
  (vector (make-hash-table :test #'eq)
          (make-hash-table :test #'eq)
          (let ((refs (make-vector (length cvec) nil)))
            (dotimes (i (length cvec))
              (aset refs i (cons (aref cvec i) i)))
            refs)
          cvec))
(defsubst goism--ir-env-tags (env) (aref env 0))
(defsubst goism--ir-env-vars (env) (aref env 1))
(defsubst goism--ir-env-consts (env) (aref env 2))
(defsubst goism--ir-env-cvec (env) (aref env 3))
(defun goism--ir-env-tag-ref (env id)
  (let ((tags (goism--ir-env-tags env)))
    (or (gethash id tags)
        (puthash id (list 'TAG (hash-table-count tags)) tags))))
(defun goism--ir-env-var-ref (env id)
  (let ((vars (goism--ir-env-vars env)))
    (or (gethash id vars)
        (puthash id
                 (cons (aref (goism--ir-env-cvec env) id) id)
                 vars))))
(defsubst goism--ir-env-const-ref (env id)
  (aref (goism--ir-env-consts env) id))

(defconst goism--ir-table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (x '(;; - Special instructions -
                 (label label ir-label)
                 (var-ref var-ref byte-varref)
                 (var-set var-set byte-varset)
                 (constant constant byte-constant)
                 ;; - Combined instructions -
                 (stack-ref stack-ref)
                 (discard discard)
                 (concat concat [nil
                                 nil
                                 byte-concat2
                                 byte-concat3
                                 byte-concat4])
                 (list list [nil
                             byte-list1
                             byte-list2
                             byte-list3
                             byte-list4])
                 ;; - Jump instructions -
                 (goto jmp)
                 (goto-if-nil jmp)
                 (goto-if-not-nil jmp)
                 (goto-if-nil-else-pop jmp)
                 (goto-if-not-nil-else-pop jmp)
                 ;; - Instructions with argument -
                 (call op1)
                 (stack-set op1)
                 ;; - Instructions without argument -
                 (setcar op0)
                 (setcdr op0)
                 (memq op0)
                 (member op0)
                 (car op0)
                 (cdr op0)
                 (array-ref op0 byte-aref)
                 (array-set op0 byte-aset)
                 (substr op0 byte-substring)
                 (str= op0 byte-string=)
                 (str< op0 byte-string<)
                 (to-lower op0 byte-downcase)
                 (to-upper op0 byte-upcase)
                 (cons? op0 byte-consp)
                 (str? op0 byte-stringp)
                 (int? op0 byte-integerp)
                 (symbol? op0 byte-symbolp)
                 (add1 op0)
                 (sub1 op0)
                 (add op0 byte-plus)
                 (sub op0 byte-diff)
                 (mul op0 byte-mult)
                 (quo op0)
                 (num= op0 byte-eqlsign)
                 (num> op0 byte-gtr)
                 (num< op0 byte-lss)
                 (num>= op0 byte-geq)
                 (num<= op0 byte-leq)
                 (neg op0 byte-negate)
                 (max op0)
                 (min op0)
                 (rem op0)
                 (eq op0)
                 (equal op0)
                 (not op0)
                 (cons op0)
                 (length op0)
                 (return op0)))
      (let* ((instr (nth 0 x))
             (kind (nth 1 x))
             (data (or (nth 2 x)
                       (intern (format "byte-%s" instr)))))
        (puthash instr (goism--ir-make-info kind data) table)))
    table))

(defun goism--ir-to-lapcode (pkg cvec)
  (let ((env (goism--ir-make-env cvec))
        op
        op-info
        arg
        output)
    (while (not-eq 'end (setq op (pop! pkg)))
      (setq op-info (gethash op goism--ir-table)
            arg (if (eq 'op0 (goism--ir-info-kind op-info))
                    nil
                  (pop! pkg)))
      (push (goism--ir-lap-instr env op-info op arg) output))
    (nreverse output)))

(defun goism--ir-lap-instr (env op-info op arg)
  ;; Patterns in `pcase' are sorted by frequency order.
  (pcase (goism--ir-info-kind op-info)
    (`op0 (list (goism--ir-info-data op-info)))
    (`op1 (let ((lap-op (goism--ir-info-data op-info)))
            (cons lap-op arg)))
    (`constant (cons 'byte-constant (goism--ir-env-const-ref env arg)))
    (`stack-ref (if (= arg 0)
                    (list 'byte-dup)
                  (cons 'byte-stack-ref arg)))
    (`discard (if (= arg 1)
                  (list 'byte-discard)
                (cons 'byte-discardN arg)))
    (`label (goism--ir-env-tag-ref env arg))
    (`jmp (let* ((lap-op (goism--ir-info-data op-info))
                 (tag (goism--ir-env-tag-ref env arg)))
            (cons lap-op tag)))
    (`var-ref (let ((var (goism--ir-env-var-ref env arg)))
                (cons 'byte-varref var)))
    (`var-set (let ((var (goism--ir-env-var-ref env arg)))
                (cons 'byte-varset var)))
    (`concat (if (and (<= arg 4) (/= 1 arg))
                 (list (aref (goism--ir-info-data op-info) arg))
               (cons 'byte-concatN arg)))
    (`list (if (<= arg 4)
               (list (aref (goism--ir-info-data op-info) arg))
             (cons 'byte-listN arg)))
    (_
     (error "Unexpected op kind for `%s'" op))))

;; {{ end }}
