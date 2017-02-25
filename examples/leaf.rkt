#lang lindenmayer racket

## axiom ##
A(0)

## rules ##
A(d) : d > 0 -> A(d+s)
A(d) : d = 0 -> F(1)[+A(D)][-A(D)]F(1)A(0)
F(a)         -> F(a*R)

## variables ##
D=1
R=1.5

n=5

s=-1
θ=45
h=800
w=800

============================================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))
(define (A turtles variables . _) turtles)
