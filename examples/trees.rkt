#lang lindenmayer racket
## axiom ##
A(1,0.1)
## rules ##
A(l,w) → !(w)F(l)[&(a)B(l*s,w*y)]/(d)A(l*r,w*y)
B(l,w) → !(w)F(l)[-(b)$C(l*s,w*y)]C(l*r,w*y)
C(l,w) → !(w)F(l)[+(b)$B(l*s,w*y)]B(l*r,w*y)
## variables ##
n=10
r=0.9
s=0.6
a=45
b=45
d=136.75
y=0.707

w=2000
h=2000

========================================

#|
I don't understand the axioms of this one.
The remainder of the symbols are the same as usual.
|#

(provide (all-defined-out)
         (all-from-out "linden-3d-turtle.rkt"))
(require "linden-3d-turtle.rkt"
         (prefix-in r: racket)
         (except-in pict3d move))

(define (A state variables . v) state)
(define (B state variables . v) state)
(define (C state variables . v) state)
(define (start variables)
  (make-turtle (dir 0 0 -3) +z +x))


(define camera (basis 'camera (point-at (pos 1.3 3.9 0) (pos 0 0 .01) #:up +x)))
(define (finish turtles variables)
  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #f
   ;#:debug? #t
   )
  (combine
   camera
   (draw turtles (vector (rgba "brown") (rgba "saddlebrown") (rgba "chocolate")))))