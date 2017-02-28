#lang lindenmayer racket

## axiom ##
X

## rules ##
Y -> [X]+X
X -> F-[Y]+F[+FX]-X
F -> FF
## variables ##
n=8
θ=22.5

=============================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) X Y)
(define (X turtles variables) turtles)
(define Y X)
