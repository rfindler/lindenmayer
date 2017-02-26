#lang lindenmayer racket

## axiom ##
X

## rules ##
X -> F[+X]F[-X]+X
F -> FF

## variables ##
n=7
θ=20

=============================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) X)
(define (X turtles variables) turtles)
