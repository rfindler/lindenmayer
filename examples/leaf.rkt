#lang lindenmayer racket

## axiom ##
A(0)

## rules ##
A(d) : d > 0 -> A(d+s)
A(d) : d = 0 -> F(1)[+A(D)][-A(D)]F(1)A(0)
F(a)         -> F(a*R)

## variables ##
D=0
R=2.00

n=10

s=-1
θ=45
------------------------------------------------------------

## axiom ##
A(0)

## rules ##
A(d) : d > 0 -> A(d+s)
A(d) : d = 0 -> F(1)[+A(D)][-A(D)]F(1)A(0)
F(a)         -> F(a*R)

## variables ##
D=1
R=1.50

n=16

s=-1
θ=45
------------------------------------------------------------

## axiom ##
A(0)

## rules ##
A(d) : d > 0 -> A(d+s)
A(d) : d = 0 -> F(1)[+A(D)][-A(D)]F(1)A(0)
F(a)         -> F(a*R)

## variables ##
D=2
R=1.36

n=21

s=-1
θ=45
------------------------------------------------------------

## axiom ##
A(0)

## rules ##
A(d) : d > 0 -> A(d+s)
A(d) : d = 0 -> F(1)[+A(D)][-A(D)]F(1)A(0)
F(a)         -> F(a*R)

## variables ##
D=4
R=1.23

n=30

s=-1
θ=45
------------------------------------------------------------

## axiom ##
A(0)

## rules ##
A(d) : d > 0 -> A(d+s)
A(d) : d = 0 -> F(1)[+A(D)][-A(D)]F(1)A(0)
F(a)         -> F(a*R)

## variables ##
D=7
R=1.17

n=45

s=-1
θ=45

============================================================
;; Section 5.3 Models of compound leaves
;; 129/141 Figure 5.11: Compound leaves
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))
(define (A turtles variables . _) turtles)
