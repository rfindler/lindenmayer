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

dist=3.5
rot=0
shift=-3.2
-------------------------------------------

## axiom ##
A(1,0.1)
## rules ##
A(l,w) → !(w)F(l)[&(a)B(l*s,w*y)]/(d)A(l*r,w*y)
B(l,w) → !(w)F(l)[-(b)$C(l*s,w*y)]C(l*r,w*y)
C(l,w) → !(w)F(l)[+(b)$B(l*s,w*y)]B(l*r,w*y)
## variables ##
n=10
r=0.9
s=0.9
a=45
b=45
d=136.75
y=0.707

w=2000
h=2000

dist=7
rot=0
shift=-3

-------------------------------------------

## axiom ##
A(1,0.1)
## rules ##
A(l,w) → !(w)F(l)[&(a)B(l*s,w*y)]/(d)A(l*r,w*y)
B(l,w) → !(w)F(l)[-(b)$C(l*s,w*y)]C(l*r,w*y)
C(l,w) → !(w)F(l)[+(b)$B(l*s,w*y)]B(l*r,w*y)
## variables ##
n=10
r=0.9
s=0.7
a=30
b=-30
d=136.75
y=0.707

w=2000
h=2000

dist=4.1
rot=-60
shift=-3
-----------------------------------------
## axiom ##
A(1,0.1)
## rules ##
A(l,w) → !(w)F(l)[&(a)B(l*s,w*y)]/(d)A(l*r,w*y)
B(l,w) → !(w)F(l)[-(b)$C(l*s,w*y)]C(l*r,w*y)
C(l,w) → !(w)F(l)[+(b)$B(l*s,w*y)]B(l*r,w*y)
## variables ##
n=10
r=0.8
s=0.7
a=31
b=-29
d=136.75
y=0.707

w=2000
h=2000

dist=3.2
rot=80
shift=-2
========================================

#|
I don't understand the axioms of this one.
The remainder of the symbols are the same as usual.
|#

(provide (all-defined-out)
         (all-from-out lindenmayer/3d-turtle))
(require lindenmayer/3d-turtle
         (except-in pict3d move))

(define (A state variables . v) state)
(define (B state variables . v) state)
(define (C state variables . v) state)
(define (start variables)
  (make-turtle (dir 0 0 (hash-ref variables 'shift)) +z +x))


(define (finish turtles variables)
    
  (define dist (hash-ref variables 'dist))
  (define v (angles->dir (hash-ref variables 'rot) 0))
  (define camera (basis 'camera (point-at (pos+ origin (dir-scale v dist)) (pos 0 0 .01) #:up +x)))
  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #f
   ;#:debug? #t
   )
  (combine
   camera
   (draw turtles (vector (rgba "brown") (rgba "saddlebrown") (rgba "chocolate")))))
