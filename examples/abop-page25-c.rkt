#lang lindenmayer racket

## axiom ##
F

## rules ##
F -> FF-
     [-F+F+F]+
     [+F-F-F]

## variables ##
n=4
θ=22.5

===================================
(require lindenmayer/turtle)
(provide
 (all-from-out lindenmayer/turtle))
