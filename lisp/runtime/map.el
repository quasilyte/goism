;; {{ define "rt/map" }}
;; Go `map' data type emulation.

(defun Go--make-map ()
  (make-hash-table :test #'equal))
(defun Go--make-map-cap (cap)
  (make-hash-table :size cap :test #'equal))

(defun Go--map-insert (key val m)
  (if (not-eq Go--nil-map m)
      (puthash key val m)
    (Go--panic "assignment to entry in nil map")))

;; {{ end }}
