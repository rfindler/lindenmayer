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
backup=900
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
backup=1000
---------------------------------------
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
backup=900
===========================
(provide (all-defined-out)
         (all-from-out lindenmayer/3d-turtle))
(require lindenmayer/3d-turtle
         lindenmayer/3d
         (except-in pict3d move)
         (prefix-in pict: pict))

#| From ABOP |#

#|

`A` creates a branch. A branch grows forward, then rotates around to create three branches (with the
stuff in []s).
The F and ! rewrite rule makes older (closer to the trunk) longer and thicker, since the earlier they
are introduced to the system the more rewrite that occur.

In essence this models how the tree actually grows: over time a given branch gets thicker and longer,
which pushes out the attached branches.

Intersting bit about this mode: It defines a vector (T1,T2,T3) that defines the tropism (the
tendency twards deformation the plant has as it grows) and an resistance to that deformation e.
this deformation is applied each time the turtle moves.
|#

(define (A state vars . _) state)
(define (start variables)
  (make-turtle (dir 0 -700 0)
               +y +x))

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
  


(define (finish turtles variables)
  (define camera (point-at (pos 0 -1 (hash-ref variables 'backup)) (pos 0 0 0)))
  (pict:scale
   (draw-pict turtles camera)
   1/3))
