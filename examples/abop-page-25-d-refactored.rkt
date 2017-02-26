#lang lindenmayer racket

## axiom ##
X

## rules ##
R -> [+X]F[-X]
X -> FR+X
F -> FF

## variables ##
n=14
θ=20

=============================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) X R)
(define (X turtles variables) turtles)
(define (R t v) t)
