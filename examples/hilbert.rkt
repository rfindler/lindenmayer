#lang lindenmayer racket
## axiom ##
X

## rules ##
X → ^ < X F ^ < X F X - F ^ > > X F X & F + > > X F X - F > X - >

## variables ##
n=3
δ=90
w=500
h=250

============================================================

(provide (all-defined-out))
(require "3d-turtle.rkt"
         (prefix-in r: racket)
         (only-in pict3d dir-scale dir+ zero-dir +x +z))

;; turn left
(define (+ state variables)
  (define δ (hash-ref variables 'δ))
  (cons (yaw (first state) δ) (rest state)))
;; turn right
(define (- state variables)
  (define δ (hash-ref variables 'δ))
  (cons (yaw (first state) (r:- δ)) (rest state)))
;; pitch down
(define (& state variables)
  (define δ (hash-ref variables 'δ))
  (cons (pitch (first state) δ) (rest state)))
;; pitch up
(define (^ state variables) (void)
  (define δ (hash-ref variables 'δ))
  (cons (pitch (first state) (r:- δ)) (rest state)))
;; roll right
(define (< state variables)
  (define δ (hash-ref variables 'δ))
  (cons (roll (first state) δ) (rest state)))
(define (> state variables)
  (define δ (hash-ref variables 'δ))
  (cons (roll (first state) (r:- δ)) (rest state)))

;; move
(define (F state variables)
  (match-define (turtle pos dir up) (first state))
  (define pos* (dir+ pos dir))
  (cons (make-turtle pos* dir up) state))

(define (X state variables) state)


(define (start variables)
  (list (turtle zero-dir +x +z)))
(define (finish turtles variables)
  (draw turtles))
