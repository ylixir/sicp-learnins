#lang sicp
;run with
; raco pkg install sicp
; racket -I sicp

(define (new-if predicate
                then-clause
                else-clause)
    (cond (predicate then-clause)
        (else else-clause)))

