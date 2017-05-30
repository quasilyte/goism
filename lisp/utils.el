;;; -*- lexical-binding: t -*-
;; {{ define "utils" }}

(eval-when-compile
  ;; Like `pop', but mutates list cells.
  (defmacro pop! (list)
    `(prog1
         (car ,list)
       (setcar ,list (nth 1 ,list))
       (setcdr ,list (cddr ,list))))
  (defmacro not-eq (x y)
    `(not (eq ,x ,y))))

;; {{ end }}
