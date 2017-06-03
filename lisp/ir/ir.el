;; {{ define "ir/ir" }}
;; Go.el IR compiler.

;; Output IR package PKG to temp buffer.
;; Caller can decide to inspect/eval/save generated contents.
;; PKG is consumed.
(defun Go--ir-pkg-compile (pkg)
  (with-output-to-temp-buffer Go-output-buffer-name
    (let ((pkg-name (pop! pkg))
          (pkg-comment (pop! pkg)))
      (Go--ir-pkg-write-header pkg-name)
      (when (not (string= "" pkg-comment))
        (Go--ir-pkg-write-comment pkg-comment))
      (Go--ir-pkg-write-body pkg)
      (Go--ir-pkg-write-footer pkg-name))
    (with-current-buffer standard-output
      (emacs-lisp-mode)
      (setq buffer-read-only t))))

(defun Go--ir-pkg-write-header (pkg-name)
  (princ ";;; -*- lexical-binding: t -*-\n")
  (princ (format ";;; %s --- translated Go package\n" pkg-name))
  (princ ";; THIS CODE IS GENERATED, AVOID MANUAL EDITING!\n"))

(defun Go--ir-pkg-write-comment (pkg-comment)
  (princ "\n;;; Commentary:\n")
  (princ pkg-comment)
  (princ "\n\n;;; Code:\n"))

(defun Go--ir-pkg-write-footer (pkg-name)
  (princ (format "\n;;; %s ends here" pkg-name)))

(defun Go--ir-pkg-write-body (pkg)
  (let (token)
    (while (setq token (pop! pkg))
      (pcase token
        (`fn (Go--ir-pkg-write-fn pkg))
        (`vars (Go--ir-pkg-write-vars pkg))
        (`expr (Go--ir-pkg-write-expr pkg))
        (_ (error "Unexpected token `%s'" token))))))

(defun Go--ir-pkg-write-fn (pkg)
  (let* ((name (pop! pkg))
         (body (Go--ir-fn-body pkg)))
    (prin1 `(defalias ',name ,body))
    (terpri)))

(defun Go--ir-pkg-write-vars (pkg)
  (let (name)
    (while (not-eq 'end (setq name (pop! pkg)))
      (prin1 `(defvar ,name nil ""))
      (terpri))))

(defun Go--ir-pkg-write-expr (pkg)
  (let* ((cvec (pop! pkg))
         (stack-cap (pop! pkg))
         (bytecode (Go--ir-to-bytecode pkg)))
    (prin1 `(byte-code ,bytecode ,cvec ,stack-cap))
    (terpri)))

(defun Go--ir-to-bytecode (pkg)
  (byte-compile-lapcode
   (byte-optimize-lapcode
    (Go--ir-to-lapcode pkg))))

(defun Go--ir-fn-body (pkg)
  (let* ((args-desc (pop! pkg))
         (cvec (pop! pkg))
         (stack-cap (pop! pkg))
         (doc-string (pop! pkg)))
    (make-byte-code args-desc
                    (Go--ir-to-bytecode pkg)
                    cvec
                    stack-cap
                    doc-string)))

(defsubst Go--ir-make-info (kind data) (cons kind data))
(defsubst Go--ir-info-kind (info) (car info))
(defsubst Go--ir-info-data (info) (cdr info))

(defconst Go--ir-table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (x '(;; - Special instructions -
                 (label label ir-label)
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
                 (constant op1)
                 (call op1)
                 (stack-set op1)
                 (var-ref op1 byte-varref)
                 (var-set op1 byte-varset)
                 ;; - Instructions without argument -
                 (setcar op0)
                 (setcdr op0)
                 (car op0)
                 (cdr op0)
                 (array-ref op0 byte-aref)
                 (array-set op0 byte-aset)
                 (substr op0 byte-substring)
                 (str= op0)
                 (str< op0)
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
        (puthash instr (Go--ir-make-info kind data) table)))
    table))

(defun Go--ir-to-lapcode (pkg)
  (let ((tags (make-hash-table :test #'eq))
        op
        op-info
        arg
        output)
    (while (not-eq 'end (setq op (pop! pkg)))
      (setq op-info (gethash op Go--ir-table)
            arg (if (eq 'op0 (Go--ir-info-kind op-info))
                    nil
                  (pop! pkg)))
      (push (Go--ir-lap-instr tags op-info op arg) output))
    (nreverse output)))

(defun Go--ir-lap-instr (tags op-info op arg)
  ;; Patterns in `pcase' are sorted by frequency order.
  (pcase (Go--ir-info-kind op-info)
    (`op0 (list (Go--ir-info-data op-info)))
    (`op1 (let ((lap-op (Go--ir-info-data op-info)))
            (cons lap-op arg)))
    (`stack-ref (if (= arg 0)
                    (list 'byte-dup)
                  (cons 'byte-stack-ref arg)))
    (`discard (if (= arg 1)
                  (list 'byte-discard)
                (cons 'byte-discardN arg)))
    (`label (Go--ir-tag-ref tags arg))
    (`jmp (let* ((lap-op (Go--ir-info-data op-info))
                 (tag (Go--ir-tag-ref tags arg)))
            (cons lap-op tag)))
    (`concat (if (and (<= arg 4) (/= 1 arg))
                 (list (aref (Go--ir-info-data op-info) arg))
               (cons byte-concatN arg)))
    (`list (if (<= arg 4)
               (list (aref (Go--ir-info-data op-info) arg))
             (cons 'byte-listN arg)))
    (_
     (error "Unexpected op kind for `%s'" op))))

(defun Go--ir-tag-ref (tags id)
  (or (gethash id tags)
      (puthash id (list 'TAG (hash-table-count tags)) tags)))

;; {{ end }}
