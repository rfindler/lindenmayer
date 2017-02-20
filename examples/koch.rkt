#lang lindenmayer racket

## axiom ##
F

## rules ##
F -> F + F - F - F + F

## variables ##
n=5

============================================================
(provide (all-defined-out))
(require graphics/value-turtles)

(define (F turtles variables)
  (draw 2 turtles))
(define (+ turtles variables)
  (turn 90 turtles))
(define (- turtles variables)
  (turn -90 turtles))

(define (start variables)
  (define w 500)
  (define h 250)
  (turn -90 (move (* h -1/2) (turn 90 (move (* w -1/2) (turtles w h))))))

(define (finish turtles variables) (clean turtles))