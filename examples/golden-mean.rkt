#lang lindenmayer typed/racket

## axiom ##
A

## rules ##
A -> AB
B -> A

## variables ##
n=20

=====================================

(provide (all-defined-out))

(: start (-> (HashTable Symbol Real)
             (Pair Natural Natural)))
(define (start variables)
  (cons 0 0))

(: finish (-> (Pair Natural Natural)
              (HashTable Symbol Real)
              Real))
(define (finish pr variables)
  (/ (car pr) (cdr pr)))

(: A (-> (Pair Natural Natural)
         (HashTable Symbol Real)
         (Pair Natural Natural)))
(define (A pr variables)
  (cons (+ (car pr) 1)
        (cdr pr)))

(: B (-> (Pair Natural Natural)
         (HashTable Symbol Real)
         (Pair Natural Natural)))
(define (B pr variables)
  (cons (car pr)
        (+ (cdr pr) 1)))
