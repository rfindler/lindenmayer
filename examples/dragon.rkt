#lang lindenmayer racket

## axiom ##
F X

## rules ##
X -> X + Y F +
Y -> - F X - Y

## variables ##
n=10

============================================================

(provide (all-defined-out))
(require graphics/value-turtles pict)

(define (X turtles variables) turtles)
(define (Y turtles variables) turtles)
(define (F turtles variables)
  (draw 4 turtles))
(define (+ turtles variables)
  (turn -90 turtles))
(define (- turtles variables)
  (turn 90 turtles))

(define w 500)
(define h 250)

(define (start variables) (turn 90 (turtles w h)))
(define (finish turtles variables)
  (inset (turtles-pict turtles) 1))
