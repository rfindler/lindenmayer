#lang lindenmayer racket
## axiom ##

!(width)[A][B]

## rules ##
A → !(width)[+A{.].C.}
B → !(width)[-B{.].C.}
C → GC

## variables ##
n=10
δ=10
width=0.05
dist=7
rot=0
shift=-5
w=2000
h=2000
---------------------------------
## axiom ##

!(width)[A][B]

## rules ##
A → !(width)[+A{.].C.}
B → !(width)[-B{.].C.}
C → GC

## variables ##
n=12
δ=10
width=0.05
dist=10
rot=0
shift=-7
w=2000
h=2000
--------------------------------
## axiom ##

!(width)[A][B]

## rules ##
A → !(width)[+A{.].C.}
B → !(width)[-B{.].C.}
C → GC

## variables ##
n=20
δ=10
width=0.1
dist=20
rot=0
shift=-10
w=2000
h=2000
========================

(provide (all-defined-out)
         (all-from-out lindenmayer/3d-turtle))
(require lindenmayer/3d-turtle
         (except-in pict3d move))

(define (A state vars) state)
(define (B state vars) state)
(define (C state vars) state)

(define (start vars)
  (make-turtle (dir 0 0 (hash-ref vars 'shift)) +z +x))


(define (finish turtles variables)
  (define dist (hash-ref variables 'dist))
  (define v (angles->dir (hash-ref variables 'rot) 00))
  (define camera (basis 'camera (point-at (pos+ origin (dir-scale v dist)) (pos 0 0 .01) #:up +x)))

  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #t)
  (combine camera (draw turtles (vector (rgba "green")))))
