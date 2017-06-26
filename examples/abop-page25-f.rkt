#lang lindenmayer racket

## axiom ##
X

## rules ##
F -> FF
X -> F-[[X]+X]+F[+FX]-X

## variables ##
n=6
θ=22.5

=============================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) X)
(define (X turtles variables) turtles)
