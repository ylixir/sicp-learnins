#lang sicp
;run with
; raco pkg install sicp
; racket -I sicp

(define (new-if predicate
                then-clause
                else-clause)
    (cond (predicate then-clause)
        (else else-clause)))

;this infinitely recurses when trying to evaluate the function call in new-if
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
