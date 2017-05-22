;;; -*- lexical-binding: t -*-

;; ir -- Go.el intermediate format that is converted to Emacs lapcode.

(eval-when-compile
  ;; Like `pop', but mutates list cells.
  (defmacro pop! (list)
    `(prog1
         (car ,list)
       (setcar ,list (nth 1 ,list))
       (setcdr ,list (cddr ,list))))
  (defmacro not-eq (x y)
    `(not (eq ,x ,y))))

;; { IR package }
;; Output of Go.el compiler.

(defmacro defun-ir (name args depth cvec &rest instrs)
  (declare (indent defun))
  (let* ((instrs (nconc instrs '(end)))
         (args-desc (byte-compile-make-args-desc args))
         (bytecode (byte-compile-lapcode
                    (byte-optimize-lapcode
                     (ir--to-lapcode instrs)))))
    `(defalias ',name
       ,(make-byte-code args-desc
                        bytecode
                        cvec
                        depth))))

;; Output IR package PKG to temp buffer.
;; Caller can decide to inspect/eval/save generated contents.
;; PKG is consumed.
(defun ir--pkg-compile (pkg)
  (with-output-to-temp-buffer "*IR compile*"
    (let ((pkg-name (pop! pkg))
          (pkg-comment (pop! pkg)))
      (ir--pkg-write-header pkg-name pkg)
      (when (not (string= "" pkg-comment))
        (ir--pkg-write-comment pkg-comment))
      (ir--pkg-write-body pkg)
      (ir--pkg-write-footer pkg-name))
    (with-current-buffer standard-output
      (emacs-lisp-mode)
      (setq buffer-read-only t))))

(defun ir--pkg-write-header (pkg-name pkg)
  (princ ";;; -*- lexical-binding: t -*-\n")
  (princ (format ";;; %s --- translated Go package\n" pkg-name))
  (princ ";; THIS CODE IS GENERATED, AVOID MANUAL EDITING!\n"))

(defun ir--pkg-write-comment (pkg-comment)
  (princ "\n;;; Commentary:\n")
  (princ pkg-comment)
  (princ "\n\n;;; Code:\n"))

(defun ir--pkg-write-footer (pkg-name)
  (princ (format "\n;;; %s ends here" pkg-name)))

(defun ir--pkg-write-body (pkg)
  (let (token)
    (while (setq token (pop! pkg))
      (pcase token
        (`fn (ir--pkg-write-fn pkg))
        (`vars (ir--pkg-write-vars pkg))
        (`expr (ir--pkg-write-expr pkg))
        (_ (error "Unexpected token `%s'" token))))))

(defun ir--pkg-write-fn (pkg)
  (let* ((name (pop! pkg))
         (body (ir--fn-body pkg)))
    (prin1 `(defalias ',name ,body))
    (terpri)))

(defun ir--pkg-write-vars (pkg)
  (let (name)
    (while (not-eq 'end (setq name (pop! pkg)))
      (prin1 `(defvar ,name nil ""))
      (terpri))))

(defun ir--pkg-write-expr (pkg)
  (let* ((cvec (pop! pkg))
         (stack-cap (pop! pkg))
         (bytecode (ir--to-bytecode pkg)))
    (prin1 `(byte-code ,bytecode ,cvec ,stack-cap))
    (terpri)))

;; { Bytecode generation }

(defun ir--to-bytecode (pkg)
  (byte-compile-lapcode
   (byte-optimize-lapcode
    (ir--to-lapcode pkg))))

(defun ir--fn-body (pkg)
  (let* ((args-desc (pop! pkg))
         (cvec (pop! pkg))
         (stack-cap (pop! pkg))
         (doc-string (pop! pkg)))
    (make-byte-code args-desc
                    (ir--to-bytecode pkg)
                    cvec
                    stack-cap
                    doc-string)))

(defsubst ir--make-info (kind data) (cons kind data))
(defsubst ir--info-kind (info) (car info))
(defsubst ir--info-data (info) (cdr info))

(defconst ir--table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (x '(;; - Special instructions -
                 (label label ir-label)
                 ;; - Combined instructions -
                 (stack-ref stack-ref)
                 (discard discard)
                 (concat comb5 [nil
                                nil
                                byte-concat2
                                byte-concat3
                                byte-concat4
                                byte-concatN])
                 (list comb5 [nil
                              byte-list1
                              byte-list2
                              byte-list3
                              byte-list4
                              byte-listN])
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
                 (string= op0)
                 (string< op0)
                 (to-lower op0 byte-downcase)
                 (to-upper op0 byte-upcase)
                 (cons? op0 byte-consp)
                 (string? op0 byte-stringp)
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
                 (neg op0)
                 (max op0)
                 (min op0)
                 (rem op0)
                 (eq op0)
                 (equal op0)
                 (not op0)
                 (cons op0)
                 (return op0)))
      (let* ((instr (nth 0 x))
             (kind (nth 1 x))
             (data (or (nth 2 x)
                       (intern (format "byte-%s" instr)))))
        (puthash instr (ir--make-info kind data) table)))
    table))

(defun ir--to-lapcode (pkg)
  (let ((tags (make-hash-table :test #'eq))
        op
        op-info
        arg
        output)
    (while (not-eq 'end (setq op (pop! pkg)))
      (setq op-info (gethash op ir--table)
            arg (if (eq 'op0 (ir--info-kind op-info))
                    nil
                  (pop! pkg)))
      (push (ir--lap-instr tags op-info op arg) output))
    (nreverse output)))

(defun ir--lap-instr (tags op-info op arg)
  ;; Patterns in `pcase' are sorted by frequency order.
  (pcase (ir--info-kind op-info)
    (`op0 (list (ir--info-data op-info)))
    (`op1 (let ((lap-op (ir--info-data op-info)))
            (cons lap-op arg)))
    (`stack-ref (if (= arg 0)
                    (list 'byte-dup)
                  (cons 'byte-stack-ref arg)))
    (`discard (if (= arg 1)
                  (list 'byte-discard)
                (cons 'byte-discardN arg)))
    (`label (ir--tag-ref tags arg))
    (`jmp (let* ((lap-op (ir--info-data op-info))
                 (tag (ir--tag-ref tags arg)))
            (cons lap-op tag)))
    (`comb5 (if (<= arg 4)
                (list (aref (ir--info-data op-info) arg))
              (cons (aref (ir--info-data op-info) 5) arg)))
    (_
     (error "Unexpected op kind for `%s'" op))))

(defun ir--tag-ref (tags id)
  (or (gethash id tags)
      (puthash id (list 'TAG (hash-table-count tags)) tags)))
