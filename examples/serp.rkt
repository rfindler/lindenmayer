#lang lindenmayer racket

## axiom ##
F - G - G
 
## rules ##
F -> F - G + F + G - F
G -> G G

## variables ##
n=6
θ=120

============================================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) G)
(define G F)
