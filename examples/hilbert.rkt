#lang lindenmayer racket
## axiom ##
A

## rules ##
A → B-F+CFC+F-D&F∧D-F+&&CFC+F+B//
B → A&F∧CFB∧F∧D∧∧-F-D∧|F∧B|FC∧F∧A//
C → |D∧|F∧B-F+C∧F∧A&&FA&F∧C+F+B∧F∧D//
D → |CFB-F+B|FA&F∧A&&FB-F+B|FC//

## variables ##
n=3
δ=90
w=500
h=250

============================================================


(provide (all-defined-out)
         (all-from-out lindenmayer/3d-turtle))

(require lindenmayer/3d-turtle
         (except-in pict3d move))


(define (A state variables) state)
(define (B state variables) state)
(define (C state variables) state)
(define (D state variables) state)

(define (start variables)
  (make-turtle zero-dir +x +z))

(define camera (basis 'camera (point-at (pos 11 2 -0.5) (pos 3 3 -3) #:up +z)))
(define (finish turtles variables)
  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #t)
  (combine camera (draw turtles)))
