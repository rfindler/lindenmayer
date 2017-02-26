#lang lindenmayer racket
## axiom ##
P


## rules ##
P → F(1)S\(180)D\(135)P
D → S
S → [+(90)F(-0.4)L]
F(n) → F(n+0.2)
L → [\(90)!(0.1)[A][B]]
A → [+(10)A{.].C.}
B → [-(10)B{.].C.}
C → GC

## variables ##
n=15
δ=30

dist=30
rot=55
shift=-10
w=2000
h=2000
========================

#| From https://www.researchgate.net/profile/Takashi_Hirano/publication/271308082_A_Geometric_Model_of_Sunflower_Plants_Using_L-system/links/562d79ba08ae04c2aeb4a67c/A-Geometric-Model-of-Sunflower-Plants-Using-L-system.pdf ABOP |#

(provide (all-defined-out)
         (all-from-out lindenmayer/3d-turtle))
(require lindenmayer/3d-turtle
         (except-in pict3d move))

(define (P state vars) state)
(define (S state vars) state)
(define (L state vars) state)
(define (A state vars) state)
(define (B state vars) state)
(define (C state vars) state)
(define (D state vars) state)

(define (start vars)
  (make-turtle (dir 0 0 (hash-ref vars 'shift)) +z +x))


(define (finish turtles variables)
  (define dist (hash-ref variables 'dist))
  (define v (angles->dir (hash-ref variables 'rot) 50))
  (define camera (basis 'camera (point-at (pos+ origin (dir-scale v dist)) (pos 0 0 .01) #:up +x)))

  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #t)
  (combine camera (draw turtles (vector (rgba "green")))))
