#lang lindenmayer racket

## axiom ##
X
## rules ##
X -> F-[[X]+X]+F[+FX]-X
F -> FF

## variables ##
n=6
θ=22.5

=============================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) X)
(define (X turtles variables) turtles)
