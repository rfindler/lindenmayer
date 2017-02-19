#lang lindenmayer racket

## axiom ##
F - G - G
 
## rules ##
F -> F - G + F + G - F
G -> G G

## variables ##
n=6

============================================================
(provide (all-defined-out))

(require graphics/value-turtles)

(define (F turtles variables)
  (draw 4 turtles))
(define (G turtles variables)
  (draw 4 turtles))
(define (- turtles variables)
  (turn 120 turtles))
(define (+ turtles variables)
  (turn -120 turtles))

(define w 260)
(define h 240)
(define (start variables)
  (turn -90 (move (* h -1/2) (turn 90 (move (* w -1/2) (turtles w h))))))
(define (finish turtles variables) (clean turtles))

