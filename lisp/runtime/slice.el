;;; -*- lexical-binding: t -*-

;; Constructors.
(defun Go--make-slice (len zero-val)
  (cons (make-vector cap zero-val)
        (cons 0 ;; offset
              (cons len len))))
(defun Go--make-slice-cap (len cap zero-val)
  (if (= len cap)
      (Go--make-slice len zero-val)
    (let ((data (make-vector cap nil)))
      (dotimes (i len)
        (aset data i zero-val))
      (cons data
            (cons 0
                  (cons len cap))))))
(defun Go--make-slice-from-list (&rest vals)
  (let* ((data (vconcat vals))
         (len (length data)))
    (cons data
          (cons 0 ;; offset
                (cons len len)))))

;; Getters.
(defmacro Go--slice-data (slice)   `(car ,slice))
(defmacro Go--slice-offset (slice) `(car (cdr ,slice)))
(defmacro Go--slice-len (slice)    `(car (cdr (cdr ,slice))))
(defmacro Go--slice-cap (slice)    `(cdr (cdr (cdr ,slice))))

;; Setters (note that slice offset is conceptually immutable).
(defmacro Go--slice-data! (slice data)
  (declare (indent 1))
  `(setcar ,slice ,data))
(defmacro Go--slice-len! (slice len)
  (declare (indent 1))
  `(setcar (cdr (cdr ,slice)) ,len))
(defmacro Go--slice-cap! (slice cap)
  (declare (indent 1))
  `(setcdr (cdr (cdr ,slice)) ,cap))

;; Elements access.
(defmacro Go--slice-get (slice index)
  `(aref (Go--slice-data ,slice)
         (+ ,index (Go--slice-offset ,slice))))
(defmacro Go--slice-set (slice index val)
  `(aset (Go--slice-data ,slice)
         (+ ,index (Go--slice-offset ,slice))
         ,val))

;; Slices without offset are consedered `fast'.
;; In practice, lack of offset means that we can use
;; slice data directly (without get/set methods).
(defmacro Go--slice-fast? (slice)
  `(= 0 (Go--slice-offset ,slice)))
(defmacro Go--slices-fast? (a b)
  `(= 0 (Go--slice-offset ,a) (Go--slice-offset ,b)))

;; Extend slice storage with new data buffer.
(defmacro Go--slice-extend (slice )
  `(progn
     (Go--slice-data! ,slice
                      (vconcat (Go--slice-data ,slice)
                               ,extra-data))
     (Go--slice-cap! ,slice
                     (+ (Go--slice-cap ,slice)
                        (length ,extra-data)))))

;; Append specialization for "append(slice, val)".
(defun Go--slice-push (slice val)
  (let ((pos (Go--slice-len slice)))
    (Go--slice-len! slice (1+ pos))
    (if (= pos (Go--slice-cap slice))
        ;; Need to extend storage.
        ;; Create a new vector with 1st element set to `val',
        ;; then re-set slice data with `vconcat' of old and new data.
        (let ((new-data (make-vector 16 nil)))
          (aset new-data 0 val)
          (Go--slice-data! slice
            (vconcat (Go--slice-data slice)
                     new-data))
          (Go--slice-cap! slice
            (+ (Go--slice-cap slice)
               (length new-data))))
      ;; Insert new value directly.
      (Go--slice-set slice pos val))
    slice))

;; Specialization that is appliable if both `dst' and `src' are `fast'.
(defun Go--slice-copy-fast (dst src)
  (let ((dst-data (Go--slice-data dst))
        (src-data (Go--slice-data src)))
    (dotimes (i (min (Go--slice-len dst)
                     (Go--slice-len src)))
      (aset dst-data i (aref src-data i)))))

;; Copy one slice contents to another.
;; Copies up to min(len(dst), len(src)) elements.
(defun Go--slice-copy (dst src)
  (if (Go--slices-fast? dst src)
      (Go--slice-copy-fast dst src)
    (dotimes (i (min (Go--slice-len dst)
                     (Go--slice-len src)))
      (Go--slice-set dst i (Go--slice-get src i)))))
