;;; -*- lexical-binding: t -*-

;; ir -- Go.el intermediate format that is converted to Emacs lapcode.

(defun ir-make-bytecode (argc code cvec stack-cap doc-string)
  "Like `make-byte-code', but CODE argument is IR encoded instruction list."
  (make-byte-code argc
                  (byte-compile-lapcode
                   (byte-optimize-lapcode
                    (ir--to-lapcode code)))
                  cvec
                  stack-cap
                  doc-string))

(defsubst ir--make-info (kind data) (cons kind data))
(defsubst ir--info-kind (info) (car info))
(defsubst ir--info-data (info) (cdr info))

(defconst ir--table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (x '(;; - Special instructions -
                 (label label ir-label)
                 ;; - Combined instructions -
                 (stack-ref comb2 ((byte-dup) . byte-stack-ref))
                 (discard comb2 ((byte-discard) . byte-discardN))
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

(defun ir--to-lapcode (instrs)
  (let ((tags (make-hash-table :test #'eq))
        op
        op-info
        output)
    (while instrs
      (setq op (pop instrs)
            op-info (gethash op ir--table))
      (pcase (ir--info-kind op-info)
        (`op0 (let ((lap-instr (ir--info-data op-info)))
                (push lap-instr output)))
        (`op1 (let ((lap-op (ir--info-data op-info))
                    (arg (pop instrs)))
                (push (cons lap-op arg) output)))
        (`jmp (let* ((lap-op (ir--info-data op-info))
                     (arg (pop instrs))
                     (tag (ir--tag-ref tags arg)))
                (push (cons lap-op tag) output)))
        (`label (let* ((id (pop instrs))
                       (tag (ir--tag-ref tags id)))
                  (push tag output)))
        (`comb2 (let ((arg (pop instrs)))
                  (if (= arg 1)
                      (push (car (ir--info-data op-info)) output)
                    (push (cons (cdr (ir--info-data op-info)) arg) output))))
        (`comb5 (let ((arg (pop instrs)))
                  (if (<= arg 4)
                      (push (aref (ir--info-data op-info) arg) output)
                    (push (cons (aref (ir--info-data op-info) 5) arg) output))))
        (_
         (error "Unexpected op kind: %s" (ir--info-kind op-info)))))
    (nreverse output)))

(defun ir--tag-ref (tags id)
  (or (gethash id tags)
      (puthash id (list 'TAG (hash-table-count tags)) tags)))
