#lang s-exp "lang.rkt"

(define (finish pr) (* 1. (if (zero? (cdr pr)) +inf.0 (/ (car pr) (cdr pr)))))
(define (A pr) (cons (+ (car pr) 1) (cdr pr)))
(define (B pr) (cons (car pr) (+ (cdr pr) 1)))

(l-system
 #:convert-start (cons 0 0)
 #:convert-finish finish
 #:iterations 20
 (A) ;; axiom
 (A -> A B)  ;; rule 1
 (B -> A))   ;; rule 2

