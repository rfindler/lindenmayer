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
         (except-in pict3d move))

;; turn left
(define (+ state variables)
  (define δ (hash-ref variables 'δ))
  (yaw state δ))
;; turn right
(define (- state variables)
  (define δ (hash-ref variables 'δ))
  (yaw state (r:- δ)))
;; pitch down
(define (& state variables)
  (define δ (hash-ref variables 'δ))
  (pitch state δ))
;; pitch up
(define (^ state variables) (void)
  (define δ (hash-ref variables 'δ))
  (pitch state (r:- δ)))
;; roll right
(define (< state variables)
  (define δ (hash-ref variables 'δ))
  (roll state δ))
(define (> state variables)
  (define δ (hash-ref variables 'δ))
  (roll state (r:- δ)))

;; move
(define (F state variables) (move state 1))

(define (X state variables) state)


(define (start variables)
  (make-turtle zero-dir +x +z))

(define camera (basis 'camera (point-at (pos 11 2 -0.5) (pos 3 3 -3) #:up +z)))
(define (finish turtles variables)
  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #t)
  (combine camera (draw turtles)))
