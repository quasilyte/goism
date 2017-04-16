(defun Go-dump-slice (slice)
  (message "len=%d cap=%d offset=%d data=%s"
           (Go-slice-len slice)
           (Go-slice-cap slice)
           (Go-slice-offset slice)
           (Go-slice-data slice)))
