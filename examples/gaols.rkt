#lang lindenmayer racket

## axiom ##

S L F(p)

## rules ##

S -> [+++G] [---H] T S

G -> +H [-G] L
H -> -G [+H] L

T -> UL
U -> V
V -> TL

L -> [-F(p)] [+F(p)] F(q)

## variables ##

p=1
q=7
n=12
θ=18

============================================================
;; This example is modified from Przemyslaw Prusinkiewicz,
;; Graphical applications of L-systems, Proceedings of Graphics
;; Interface '86, Fig. 3(f)

(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))
(define (S turtles variables) turtles)
(define (G turtles variables) turtles)
(define (H turtles variables) turtles)
(define (T turtles variables) turtles)
(define (U turtles variables) turtles)
(define (V turtles variables) turtles)
(define (L turtles variables) turtles)

