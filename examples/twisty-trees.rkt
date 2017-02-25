#lang lindenmayer racket

## axiom ##
A(1,0.1)

## rules ##
A(l,w) → !(w)F(l)[&(a)B(l*r,w*y)] /(180)[&(b)B(l*s,w*y)]
B(l,w) → !(w)F(l)[+(a)$B(l*r,w*y)] [-(b)$B(l*s,w*y)]

## variables ##
n=10
r=0.9
s=0.7
a=10
b=60
y=0.77

w=2000
h=2000
rot=-40
dist=4.7
------------------------------------------------
## axiom ##
A(1,0.1)

## rules ##
A(l,w) → !(w)F(l)[&(a)B(l*r,w*y)]/(180)[&(b)B(l*s,w*y)]
B(l,w) → !(w)F(l)[+(a)$B(l*r,w*y)][-(b)$B(l*s,w*y)]

## variables ##
n=10
r=0.9
s=0.8
a=20
b=50
y=0.77

w=2000
h=2000
rot=100
dist=5.2
------------------------------------------------
## axiom ##
A(1,0.1)

## rules ##
A(l,w) → !(w)F(l)[&(a)B(l*r,w*y)] /(180)[&(b)B(l*s,w*y)]
B(l,w) → !(w)F(l)[+(a)$B(l*r,w*y)] [-(b)$B(l*s,w*y)]

## variables ##
n=10
r=0.9
s=0.8
a=35
b=35
y=0.77

w=2000
h=2000
rot=-240
dist=4.6
===============
(provide (all-defined-out)
         (all-from-out lindenmayer/3d-turtle))
(require lindenmayer/3d-turtle
         (except-in pict3d move))

(define (A state variables . v) state)
(define (B state variables . v) state)
(define (start variables)
  (make-turtle (dir 0 0 -3) +z +x))

(define (finish turtles variables)
  
  (define dist (hash-ref variables 'dist))
  (define v (angles->dir (hash-ref variables 'rot) 0))
  (define camera (basis 'camera (point-at (pos+ origin (dir-scale v dist)) (pos 0 0 .01) #:up +x)))

  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #f)
  (combine
   camera
   (draw turtles)))
