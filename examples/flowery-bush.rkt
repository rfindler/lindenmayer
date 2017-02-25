#lang lindenmayer racket

## axiom ##
P
## rules ##
P → I+[P+F]--//[--L]I[++L]-[PF]++PF
I → FS[//&&L][//^^]LFS
S → SFS
L → ['{+f-ff-f+|+f-ff-f}]
F → [&&&p'/W////W////W////W////W////]
p → FF
W → ['^F][''{&&&&-f+f|-f+f}]
## variables ##
n=5
δ=18
w=500
h=500
dist=20
rot=45
=========================

#|
P is the plant
S is stem
E is segment
|#
(provide (all-defined-out)
         (all-from-out "linden-3d-turtle.rkt"))
(require "linden-3d-turtle.rkt"
         (prefix-in r: racket)
         (except-in pict3d move))

(define (P state vars) state)
(define (I state vars) state)
(define (S state vars) state)
(define (L state vars) state)
(define (p state vars) state)
(define (W state vars) state)

(define (start vars) (make-turtle (dir 0 0 -5) +z +x))


(define (finish turtles variables)
    
  (define dist (hash-ref variables 'dist))
  (define v (angles->dir (hash-ref variables 'rot) 0))
  (define camera (basis 'camera (point-at (pos+ origin (dir-scale v dist)) (pos 0 0 .01) #:up +x)))

  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #t
   ;#:debug? #t
   )
  (combine camera (draw turtles (vector (rgba "brown") (rgba "green") (rgba "white") (rgba "DeepPink")))))