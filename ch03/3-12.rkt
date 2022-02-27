#lang sicp

(let ((x 2) (y 3))
  (let ((x 7) (z (+ x y)))
    (* z x)))
; 35

(let* ((x 2) (y 3))
  (let* ((x 7) (z (+ x y)))
    (* z x)))
; 70

(letrec ((x 2)
         (y 3))
  (letrec ((x 7) (z (+ x y)))
    (* z x)))
; error
;   x: undefined;
;   cannot use before initialization