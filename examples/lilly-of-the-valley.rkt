#lang lindenmayer racket
## axiom ##
TSSSSS

## rules ##
T → ['Q(4)]
Q(d) → F(d)+Q(d/2)
S →[X(36)A]/(72)[X(36)B]
A → [&GA{.].
B → B&.G.}
X(a) → X(a+4.5)
## variables ##
n=18
δ=4.5
dist=20
rot=50
shift=3
w=2000
h=2000

==========================

(provide (all-defined-out)
         (all-from-out lindenmayer/3d-turtle))
(require lindenmayer/3d-turtle
         lindenmayer/3d
         (except-in pict3d move))

(define X &)
(define (T state vars) state)
(define (Q state vars d) state)
(define (S state vars) state)
(define (A state vars) state)
(define (B state vars) state)

(define (start vars)
  (make-turtle (dir 0 0 (hash-ref vars 'shift)) +z +y))


(define (finish turtles variables)
  (define dist (hash-ref variables 'dist))
  (define v (angles->dir (hash-ref variables 'rot) 00))
  (define camera (basis 'camera (point-at (pos+ origin (dir-scale v dist)) (pos 0 0 .01) #:up +x)))

  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #t #:debug? #t)
  (combine camera (draw turtles (vector (rgba "blue") (rgba "green")))))
