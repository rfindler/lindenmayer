#lang lindenmayer racket

## axiom ##

A(0, 1)

## rules ##

A(t, k) : t > 0 → A(t+s, k)
A(t, k) : t = 0 → F(1) [ -(k*α) A(6, k) ] F(1) -(1) A(0, -k)
F(x) → F(v*x)

## variables ##

l = 90
v = 1.15
c = 0.7
s = -1
θ = 3
α = 15

n = 45

------------------------------------------------------------

## axiom ##

A(0, 1)

## rules ##

A(t, k) : t > 0 → A(t+s, k)
A(t, k) : t = 0 → F(1) [ -(k*α) A(3, k) ] F(1) -(1) A(0, -k)
F(x) → F(v*x)

## variables ##

l = 100
v = 1.15
c = 0.7
s = -1
θ = 3
α = 15

n = 30

============================================================
;; Examples drawn from Winfried Kurth, Specification of morphological models with
;; L-systems and relational growth grammars, Journal of Interdisciplinary Image Science
;;
;; The L-systems are the ones in Fig. 8(a)(b) in the paper, page 9-10.
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))

(define (A turtles variables . _) turtles)
