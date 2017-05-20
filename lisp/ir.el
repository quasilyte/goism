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
    (ir--pkg-write-header (pop pkg))
    (ir--pkg-write-body pkg)))

(defun ir--pkg-write-header (pkg-name)
  (princ ";;; -*- lexical-binding: t -*-\n")
  (princ ";; THIS CODE IS GENERATED, AVOID MANUAL EDITING!\n")
  (princ (format ";; Go package %s:\n" pkg-name)))

(defun ir--pkg-write-body (pkg)
  (let (token)
    (while (setq token (pop! pkg))
      (pcase token
        (`fn (ir--pkg-write-fn pkg))
        (`var (ir--pkg-write-var pkg))
        (_ (error "Unexpected token `%s'" token)))
      (terpri))))

(defun ir--pkg-write-fn (pkg)
  (let* ((name (pop! pkg))
         (body (ir--fn-body pkg)))
    (prin1 `(defalias ',name ,body))))

(defun ir--pkg-write-var (pkg)
  (error "Unimplemented"))

;; { Bytecode generation }

(defun ir--fn-body (pkg)
  (let* ((args-desc (pop! pkg))
         (cvec (pop! pkg))
         (stack-cap (pop! pkg))
         (doc-string (pop! pkg)))
    (make-byte-code args-desc
                    (byte-compile-lapcode
                     (byte-optimize-lapcode
                      (ir--to-lapcode pkg)))
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
                 (stack-ref stack-ref (byte-dup))
                 (discard discard (byte-discard))
                 (concat comb5 [nil
                                nil
                                (byte-concat2)
                                (byte-concat3)
                                (byte-concat4)
                                byte-concatN])
                 (list comb5 [nil
                              (byte-list1)
                              (byte-list2)
                              (byte-list3)
                              (byte-list4)
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
                 (setcar op0 (byte-setcar))
                 (setcdr op0 (byte-setcdr))
                 (car op0 (byte-car))
                 (cdr op0 (byte-cdr))
                 (array-ref op0 (byte-aref))
                 (array-set op0 (byte-aset))
                 (substr op0 (byte-substring))
                 (string= op0 (byte-string=))
                 (string< op0 (byte-string<))
                 (to-lower op0 (byte-downcase))
                 (to-upper op0 (byte-upcase))
                 (cons? op0 (byte-consp))
                 (string? op0 (byte-stringp))
                 (int? op0 (byte-integerp))
                 (symbol? op0 (byte-symbolp))
                 (add1 op0 (byte-add1))
                 (sub1 op0 (byte-sub1))
                 (add op0 (byte-plus))
                 (sub op0 (byte-diff))
                 (mul op0 (byte-mult))
                 (quo op0 (byte-quo))
                 (num= op0 (byte-eqlsign))
                 (num> op0 (byte-gtr))
                 (num< op0 (byte-lss))
                 (num>= op0 (byte-geq))
                 (num<= op0 (byte-leq))
                 (neg op0 (byte-negate))
                 (max op0 (byte-max))
                 (min op0 (byte-min))
                 (rem op0 (byte-rem))
                 (eq op0 (byte-eq))
                 (equal op0 (byte-equal))
                 (not op0 (byte-not))
                 (cons op0 (byte-cons))
                 (return op0 (byte-return))))
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
  (pcase (ir--info-kind op-info)
    (`label (ir--tag-ref tags arg))
    (`op0 (ir--info-data op-info))
    (`op1 (let ((lap-op (ir--info-data op-info)))
            (cons lap-op arg)))
    (`jmp (let* ((lap-op (ir--info-data op-info))
                 (tag (ir--tag-ref tags arg)))
            (cons lap-op tag)))
    (`stack-ref (if (= arg 0)
                    (ir--info-data op-info)
                  (cons 'byte-stack-ref arg)))
    (`discard (if (= arg 1)
                  (ir--info-data op-info)
                (cons 'byte-discardN arg)))
    (`comb5 (if (<= arg 4)
                (aref (ir--info-data op-info) arg)
              (cons (aref (ir--info-data op-info) 5) arg)))
    (_
     (error "Unexpected op kind for `%s'" op))))

(defun ir--tag-ref (tags id)
  (or (gethash id tags)
      (puthash id (list 'TAG (hash-table-count tags)) tags)))
