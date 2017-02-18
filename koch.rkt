#lang s-exp "lang.rkt"

(require graphics/value-turtles)

(define (F turtles)
  (draw 2 turtles))
(define (+ turtles)
  (turn 90 turtles))
(define (- turtles)
  (turn -90 turtles))

(define w 500)
(define h 250)

(l-system
 #:convert-start
 (turn -90 (move (* h -1/2) (turn 90 (move (* w -1/2) (turtles w h)))))
 #:convert-finish values
 #:iterations 5
 (F)
 (F -> F + F - F - F + F))
