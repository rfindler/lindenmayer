#lang lindenmayer racket

## axiom ##
A(0)
## rules ##
A(d) : d > 0 -> A(d-1)
A(d) : d = 0 -> F(1/2)[+A(D)]F(1/2)B(0)
B(d) : d > 0 -> B(d-1)
B(d) : d = 0 -> F(1/2)[-B(D)]F(1/2)A(0)
F(a)         -> F(a*R)
## variables ##
D=1
R=1.36
n=20
θ=45

------------------------------------------------------------

## axiom ##
A(0)
## rules ##
A(d) : d > 0 -> A(d-1)
A(d) : d = 0 -> F(1)[+A(D)]F(1)B(0)
B(d) : d > 0 -> B(d-1)
B(d) : d = 0 -> F(1)[-B(D)]F(1)A(0)
F(a)         -> F(a*R)

## variables ##
D=4
R=1.18

n=34

θ=45

------------------------------------------------------------

## axiom ##
A(0)
## rules ##
A(d) : d > 0 -> A(d-1)
A(d) : d = 0 -> F(1)[+A(D)]F(1)B(0)
B(d) : d > 0 -> B(d-1)
B(d) : d = 0 -> F(1)[-B(D)]F(1)A(0)
F(a)         -> F(a*R)

## variables ##
D=7
R=1.13

n=46

θ=45

============================================================
;; Section 5.3 Models of compound leaves
;; 130/142 Figure 5.12: Compound leaves with alternating branching patterns
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))
(define (A turtles variables . _) turtles)
(define (B turtles variables . _) turtles)
