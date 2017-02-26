#lang lindenmayer racket

## axiom ##
F

## rules ##
F -> F[+F]F[-F][F]

## variables ##
n=5
θ=20

===========================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle))
