#lang lindenmayer racket

## axiom ##
A

## rules ##
A -> AB
B -> A

## variables ##
n=20

============================================================

(provide (all-defined-out))

(define (start variables) (cons 0 0))
(define (finish pr variables) (* 1. (if (zero? (cdr pr)) +inf.0 (/ (car pr) (cdr pr)))))
(define (A pr variables) (cons (+ (car pr) 1) (cdr pr)))
(define (B pr variables) (cons (car pr) (+ (cdr pr) 1)))
