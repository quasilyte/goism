;;; -*- lexical-binding: t -*-

;; Constructors.
(defmacro Go--slice (data offset len cap)
  `(cons ,data
         (cons ,offset
               (cons ,len ,cap))))
(defun Go--make-slice (len zero-val)
  (Go--slice (make-vector len zero-val) 0 len len))
(defun Go--make-slice-cap (len cap zero-val)
  (if (= len cap)
      (Go--make-slice len zero-val)
    (let ((data (make-vector cap nil)))
      (dotimes (i len)
        (aset data i zero-val))
      (Go--slice data 0 len cap))))
(defun Go--make-slice-from-list (&rest vals)
  (let* ((data (vconcat vals))
         (len (length data)))
    (Go--slice data 0 len len)))

;; Getters.
(defmacro Go--slice-data (slice)   `(car ,slice))
(defmacro Go--slice-offset (slice) `(car (cdr ,slice)))
(defmacro Go--slice-len (slice)    `(car (cdr (cdr ,slice))))
(defmacro Go--slice-cap (slice)    `(cdr (cdr (cdr ,slice))))

;; Setters.
(defmacro Go--slice-data! (slice data)
  (declare (indent 1))
  `(setcar ,slice ,data))
(defmacro Go--slice-offset! (slice offset)
  (declare (indent 1))
  `(setcar (cdr ,slice) ,offset))
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

;; Boundary checks.
(defmacro Go--slice-len-bound (slice index)
  `(when (and (< ,index 0)
              (> ,index (Go--slice-len ,slice)))
     (Go--panic "slice bounds out of range")))
(defmacro Go--slice-cap-bound (slice index)
  `(when (and (< ,index 0)
              (> ,index (Go--slice-cap ,slice)))
     (Go--panic "slice bounds out of range")))

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
          ;; For slices with offset with should take sub-vector
          ;; to avoid memory leaks.
          (Go--slice-data! slice
            (vconcat (if (Go--slice-fast? slice)
                         (Go--slice-data slice)
                       (substring (Go--slice-data slice)
                                  (Go--slice-offset slice)))
                     new-data))
          (Go--slice-cap! slice
                  (+ (Go--slice-cap slice)
                     (length new-data)))
          (unless (Go--slice-fast? slice)
            (Go--slice-offset! slice 0)))
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

;; Simple re-slicing.
(defun Go--subslice2 (slice low high)
  (Go--slice-len-bound slice low)
  (Go--slice-cap-bound slice high)
  (Go--slice (Go--slice-data slice)
             (+ (Go--slice-offset slice) low)
             (- high low)
             (- (Go--slice-cap slice) low)))
;; Specialization for "slice[low:]".
(defun Go--subslice-low (slice low)
  (Go--slice-len-bound slice low)
  (Go--slice (Go--slice-data slice)
             (+ (Go--slice-offset slice) low)
             (- (Go--slice-len slice) low)
             (- (Go--slice-cap slice) low)))
;; Specialization for "slice[:high]".
(defun Go--subslice-high (slice high)
  (Go--slice-cap-bound slice high)
  (Go--slice (Go--slice-data slice)
             (Go--slice-offset slice)
             high
             (Go--slice-cap slice)))

;; Slicing an array: "arr[low:high]".
(defun Go--array-slice (arr low high)
  (Go--slice arr low (- high low) (- (length arr) low)))
;; Specialization for "arr[:]".
(defun Go--array-slice-whole (arr)
  (let ((len (length arr)))
    (Go--slice arr 0 len len)))
;; Specialization for "arr[low:]".
(defun Go--array-slice-low (arr low)
  (let ((len (length arr)))
    (Go--slice arr low (- len low) (- len low))))
;; Specialization for "arr[:high]".
(defun Go--array-slice-high (arr high)
  (Go--slice arr 0 high (length arr)))

;; Convert byte slice to string: "string(slice)".
(defun Go--slice-to-str (slice)
  (if (Go--slice-fast? slice)
      (concat (Go--slice-data slice) "")
    (concat (substring (Go--slice-data slice)
                       (Go--slice-offset slice)
                       (+ (Go--slice-offset slice) (Go--slice-len slice)))
            "")))
