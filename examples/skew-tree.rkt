#lang lindenmayer racket

## axiom ##

A(1)

## rules ##

A(s) → !(s*d) F(l*s) [ -(5) A(s*c) ] [ +(1) A(s*c) ]

## variables ##

l = 94
c = 0.7
d = 10
θ = 10

n = 12
------------------------------------------------------------

## axiom ##

A(1)

## rules ##

A(s) → !(s*d) F(l*s) [ -(5) B(s*c) ] [ +(1) D(s*c) ]
B(s) → !(s*d) F(l*s) [ +(5) A(s*c) ] [ -(1) C(s*c) ]
C(s) → !(s*d) F(l*s) [ -(5) E(s*c) ] [ +(1) A(s*c) ]
D(s) → !(s*d) F(l*s) [ +(5) B(s*c) ] [ -(1) E(s*c) ]
E(s) → !(s*d) F(l*s) [ -(5) A(s*c) ] [ +(1) C(s*c) ]

## variables ##

l = 100
c = 0.7
d = 10
θ = 10

n = 12

============================================================
;; Examples taken from Winfried Kurth, Specification of morphological models with
;; L-systems and relational growth grammars, Journal of Interdisciplinary Image Science
;;
;; The first L-system is Fig. 6(a) in the paper (page 7) and the second L-system is a
;; variation of the L-system in Fig. 6(b), changing the system to be deterministic instead.
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))

(define (A turtles variables . _) turtles)
(define (B turtles variables . _) turtles)
(define (C turtles variables . _) turtles)
(define (D turtles variables . _) turtles)
(define (E turtles variables . _) turtles)
