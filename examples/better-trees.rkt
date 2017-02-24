#lang lindenmayer racket


## axiom ##
!(1)F(200)/(45)A

## rules ##
A → !(v)F(50)[&(a)F(50)A]/(d)[&(a)F(50)A]/(D)[&(a)F(50)A]
F(l) → F(l*m)
!(w) → !(w*v)
## variables ##
d=94.74
D=132.63
a=18.95
m=1.109
T1=0
T2=-1
T3=0
e=.22
v=1.732
n=6
w=500
h=500
----------------------------------------
## axiom ##
!(1)F(200)/(45)A

## rules ##
A → !(v)F(50)[&(a)F(50)A]/(d)[&(a)F(50)A]/(D)[&(a)F(50)A]
F(l) → F(l*m)
!(w) → !(w*v)
## variables ##
d=137.5
D=137.5
a=18.95
m=1.109
T1=0
T2=-1
T3=0
e=.14
n=7
v=1.732
w=500
h=500
----------------------------------------
## axiom ##
!(1)F(200)/(45)A

## rules ##
A → !(v)F(50)[&(a)F(50)A]/(d)[&(a)F(50)A]/(D)[&(a)F(50)A]
F(l) → F(l*m)
!(w) → !(w*v)
## variables ##
d=112.5
D=157.5
a=22.5
m=1.79
T1=-.02
T2=-1
T3=0
e=.27
n=8
v=1.732
w=500
h=500
----------------------------------------
## axiom ##
!(1)F(200)/(45)A

## rules ##
A → !(v)F(50)[&(a)F(50)A]/(d)[&(a)F(50)A]/(D)[&(a)F(50)A]
F(l) → F(l*m)
!(w) → !(w*v)
## variables ##
d=180
D=252
a=36
m=1.07
T1=-.61
T2=-.77
T3=-.19
e=.27
n=6
v=1.732
w=500
h=500
===========================
(provide (all-defined-out)
         (all-from-out "linden-3d-turtle.rkt"))
(require "linden-3d-turtle.rkt"
         "3d-turtle.rkt"
         (prefix-in r: racket)
         (except-in pict3d move))

(define (A state vars . _) state)
(define (start variables)
  (make-turtle zero-dir +y +z))

(define (adjustment e H T)
  (* e (dir-dist (dir-cross H T))))
  
(define (F state vars dist)
  (define state* (move state dist))
  (define H (turtle-facing state))
  (define T
    (dir (hash-ref vars 'T1)
         (hash-ref vars 'T2)
         (hash-ref vars 'T3)))
  (define e (hash-ref vars 'e))
  (define α (adjustment e H T))
  (nudge state* 1 (dir-scale (dir-normalize T) α)))
  

(define camera (basis 'camera
                          (affine-compose
                           (point-at (pos 0 -1 1500) (pos 0 0 0)))))
(define (finish turtles variables)
  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #f
   #:debug? #t
   )
  (combine camera (draw turtles)))