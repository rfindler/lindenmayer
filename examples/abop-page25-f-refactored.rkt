#lang lindenmayer racket

## axiom ##
X
## rules ##
X -> F-[Y]+F[+FX]-X
Y -> [X]+X
F -> FF
## variables ##
n=8
θ=22.5

=============================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) X Y)
(define (X turtles variables) turtles)
(define Y X)
