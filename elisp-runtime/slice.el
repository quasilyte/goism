;; Go slice.
;;
;; TODO:
;;   Out-of-bound (len) access should cause panic,
;;   but naive implementation will cause big slowdown.

(defsubst Go-make-slice (len cap)
  (let ((data (make-vector cap nil)))
    ;; 0 is offset. New slices have no offset.
    (vector data 0 len cap)))

;; Getters
(defmacro Go-slice-data (slice) `(aref ,slice 0))
(defmacro Go-slice-offset (slice) `(aref ,slice 1))
(defmacro Go-slice-len (slice) `(aref ,slice 2))
(defmacro Go-slice-cap (slice) `(aref ,slice 3))

;; Setters (note that slice offset is conceptually immutable)
(defmacro Go-slice-data! (slice data) `(aset ,slice 0 ,data))
(defmacro Go-slice-len! (slice len) `(aset ,slice 2 ,len))
(defmacro Go-slice-cap! (slice cap) `(aset ,slice 3 ,cap))

;; Slices without offset are consedered `fast'.
;; In practice, lack of offset means that we can use
;; slice data directly (without get/set methods).
(defmacro Go-slice-fast? (slice)
  `(= 0 (Go-slice-offset ,slice)))
(defmacro Go-slices-fast? (a b)
  `(= 0 (Go-slice-offset ,a) (Go-slice-offset ,b)))

(defmacro Go-slice-get/fast (slice index)
  `(aref (Go-slice-data ,slice) ,index))
(defmacro Go-slice-get (slice index)
  `(aref (Go-slice-data ,slice)
         (+ ,index (Go-slice-offset ,slice))))

(defmacro Go-slice-set/fast (slice index val)
  `(aset (Go-slice-data ,slice) ,index ,val))
(defmacro Go-slice-set (slice index val)
  `(aset (Go-slice-data ,slice)
         (+ ,index (Go-slice-offset ,slice))
         ,val))

;; Specialization for slice[offset:].
(defmacro Go-subslice/offset (slice offset)
  `(vector (Go-slice-data ,slice)
           ,offset
           (- (Go-slice-len ,slice) ,offset)
           (- (Go-slice-cap ,slice) ,offset)))
