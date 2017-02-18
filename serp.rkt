#lang s-exp "lang.rkt"

(require graphics/value-turtles)

(define (F turtles)
  (draw 4 turtles))
(define (G turtles)
  (draw 4 turtles))
(define (- turtles)
  (turn 120 turtles))
(define (+ turtles)
  (turn -120 turtles))

(define w 260)
(define h 240)

(l-system
 #:convert-start
 (turn -90 (move (* h -1/2) (turn 90 (move (* w -1/2) (turtles w h)))))
 #:convert-finish values
 #:iterations 6
 (F - G - G)
 (F -> F - G + F + G - F)
 (G -> G G))
