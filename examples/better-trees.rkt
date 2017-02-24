#lang lindenmayer racket


## axiom ##
!(1)F(200)/(45)A

## rules ##
A → !(v)F(50)[&(a)F(50)A]/(d)[&(a)F(50)A]/(e)[&(a)F(50)A]
F(l) → F(l*m)
!(w) → !(w*v)
## variables ##
d=94.74
e=132.63
a=18.95
m=1.109
v=1.732
n=6
w=1000
h=1000
----------------------------------------
## axiom ##
!(1)F(200)/(45)A

## rules ##
A → !(v)F(50)[&(a)F(50)A]/(d)[&(a)F(50)A]/(e)[&(a)F(50)A]
F(l) → F(l*m)
!(w) → !(w*v)
## variables ##
d=137.5
e=137.5
a=18.95
m=1.109
n=8
v=1.732
w=1000
h=1000
----------------------------------------
## axiom ##
!(1)F(200)/(45)A

## rules ##
A → !(v)F(50)[&(a)F(50)A]/(d)[&(a)F(50)A]/(e)[&(a)F(50)A]
F(l) → F(l*m)
!(w) → !(w*v)
## variables ##
d=112.5
e=157.5
a=22.5
m=1.79
n=8
v=1.732
w=1000
h=1000
----------------------------------------
## axiom ##
!(1)F(200)/(45)A

## rules ##
A → !(v)F(50)[&(a)F(50)A]/(d)[&(a)F(50)A]/(e)[&(a)F(50)A]
F(l) → F(l*m)
!(w) → !(w*v)
## variables ##
d=180
e=252
a=36
m=1.07
n=6
v=1.732
w=1000
h=1000
===========================
(provide (all-defined-out)
         (all-from-out "linden-3d-turtle.rkt"))
(require "linden-3d-turtle.rkt"
         (prefix-in r: racket)
         (except-in pict3d move))

(define (A state vars . _) state)
(define (start variables) (make-turtle zero-dir +z +x))


(define camera (basis 'camera (point-at (pos 29 14 17) (pos 0 0 17) #:up +z)))
(define (finish turtles variables)
  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #f
   ;#:debug? #t
   )
  (draw turtles))