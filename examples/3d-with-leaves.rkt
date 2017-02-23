#lang lindenmayer racket
## axiom ##
A
## rules ##
A → [&FL!A]/////’[&FL!A]///////’[&FL!A]
F → S/////F
S → F L
L → [’’’∧∧l]
## variables ##
n=7
δ=22.5
w=500
h=500

==================================================


(provide (all-defined-out)
         (all-from-out "linden-3d-turtle.rkt"))
(require "linden-3d-turtle.rkt"
         (prefix-in r: racket)
         (except-in pict3d move))

(define leaf
  (let ()
    (define es 1)
    (define start (vertex (pos 0 0 0)))
    (define end (vertex (pos (r:+ es 1) 0 0)))
    (define q1 (vertex (pos 1/3 -1/3 0)))
    (define q2 (vertex (pos 1/3 1/3 0)))
    (define q3 (vertex (pos (r:+ es 2/3) 1/3 0)))
    (define q4 (vertex (pos (r:+ es 2/3) -1/3 0)))
    (with-color
     (rgba "green")
     (combine
      (triangle start q1 q2)
      (triangle start q1 q2 #:back? #t)
      (quad q1 q2 q3 q4)
      (quad q1 q2 q3 q4 #:back? #t)
      (triangle end q3 q4)
      (triangle end q3 q4 #:back? #t)))))

(define (l state variables)
  (insert-pict state leaf))

;; move

(define (A state variables) state)
(define (S state variables) state)
(define (L state variables) state)

(define (start variables) (make-turtle zero-dir +z +x 1))

(define camera (basis 'camera (point-at (pos 29 14 17) (pos 0 0 17) #:up +z)))
(define (finish turtles variables)
  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #t
   ;#:debug? #t
   )
  (combine camera (draw turtles (vector (rgba "brown") (rgba "saddlebrown") (rgba "chocolate")))))