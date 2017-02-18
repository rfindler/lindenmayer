#lang s-exp "lang.rkt"

(require graphics/value-turtles)

(define (X turtles) turtles)
(define (Y turtles) turtles)
(define (F turtles)
  (draw 4 turtles))
(define (+ turtles)
  (turn -90 turtles))
(define (- turtles)
  (turn 90 turtles))

(define w 500)
(define h 250)

(l-system
 #:convert-start (turn 90 (turtles w h))
 #:convert-finish values
 #:iterations 10
 (F X)
 (X -> X + Y F +)
 (Y -> - F X - Y))
