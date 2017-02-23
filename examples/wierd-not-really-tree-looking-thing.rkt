#lang lindenmayer racket

## axiom ##
A

## rules ##
A → F(1)[+A][-A]
F(s) → F(s*R)

## variables ##
R=1.5
n=10

============================================================

(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) A)
(define (A state . args) state)
