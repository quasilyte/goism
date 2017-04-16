;; #FIXME: need to check if functions behavior conforms to specs. 

;; Specialization that is appliable if both `dst' and `src' are `fast'.
(defun Go-copy-fast (dst src)
  (let ((dst-data (Go-slice-data dst))
        (src-data (Go-slice-data src)))
    (dotimes (i (min (Go-slice-len dst)
                     (Go-slice-len src)))
      (aset dst-data i (aref src-data i)))))

(defun Go-copy (dst src)
  (if (Go-slices-fast? dsr src)
      (Go-copy-fast dst src)
    (dotimes (i (min (Go-slice-len dst)
                     (Go-slice-len src)))
      (Go-slice-set dst
                    i
                    (Go-slice-get src i)))))

;; Specialization for append(slice, elem)
(defun Go-append-elem (slice elem)
  (let ((pos (Go-slice-len slice)))
    (Go-slice-len! slice (1+ pos))
    (if (= pos (Go-slice-cap slice))
        (let ((new-data (make-vector 16 nil)))
          (aset new-data 0 elem)
          (Go-slice-data! slice
                          (vconcat (Go-slice-data slice) new-data))
          (Go-slice-cap! slice
                         (+ 16 (Go-slice-cap slice))))
      (Go-slice-set slice pos elem)))
  slice)

;; Specialization for append(dst, src...)
;; #FIXME: optimize me.
(defun Go-append-slice (dst src)
  (let ((dst-len (Go-slice-len dst))
        (src-len (Go-slice-len src)))
    (Go-slice-len! dst (+ dst-len src-len))
    (if (> src-len (- (Go-slice-cap dst) dst-len))
        (progn
          (Go-slice-data! dst
                          (vconcat (substring (Go-slice-data dst)
                                              (Go-slice-offset dst)
                                              dst-len)
                                   (substring (Go-slice-data src)
                                              (Go-slice-offset src)
                                              src-len)
                                   (make-vector 16 nil)))
          (Go-slice-cap! dst (+ 16 dst-len src-len)))
      (dotimes (i src-len)
        (Go-slice-set dst
                      (+ i dst-len)
                      (Go-slice-get src i)))))
  dst)
